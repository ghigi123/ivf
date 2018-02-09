package IVF

sealed abstract class Criterion {
  def name: String

  def build(cfg: CFG): (TestGenerator, Coverage)
}

case class AllAssignCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all assigns"

  override def build(cfg: CFG): (TestGenerator, NodeCoverage) = {
    val requiredNodes = cfg.labels.filterKeys(cfg.isAssign).keys.toSet
    (
      Criterion.criterionFromRequiredNodes(cfg, maxLoopDepth, requiredNodes),
      NodeCoverage(requiredNodes.map(Node))
    )
  }
}

case class AllDecisionsCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all decisions"

  override def build(cfg: CFG): (TestGenerator, NodeCoverage) = {
    val requiredNodes: Set[Int] = cfg.graph.edges
      .filter(e => e.label == "true" || e.label == "false")
      .map(e => e.to.value)
      .toSet

    (
      Criterion.criterionFromRequiredNodes(cfg, maxLoopDepth, requiredNodes),
      NodeCoverage(requiredNodes.map(Node))
    )
  }
}


case class AllKPathCriterion(k: Int) extends Criterion {
  override def name: String = s"all k paths with k=${k.toString}"

  override def build(cfg: CFG): (TestGenerator, PathCoverage) = {
    def lambda(nextNodeLabel: Int, localK: Int, path: Vector[Int]): (Option[Set[Vector[Int]]], Int) =
      if (localK == 0)
        (Some(Set[Vector[Int]]()), localK)
      else
        (None, localK - 1)

    val paths: Seq[Vector[Int]] = cfg.forwardPathBuilderAux(0, Vector(0), Int.MaxValue, cfg.emptyLoopStates, k, lambda).toVector

    (
      TestGeneratorMap(cfg, "path", All, paths.map(path => Path(path).toString -> PathTestGenerator(cfg, "constraint", path)).toMap),
      PathCoverage(paths.toSet.map(Path))
    )
  }
}

case class AllILoopsCriterion(i: Int) extends Criterion {
  override def name: String = s"all i loops with i=${i.toString}"

  override def build(cfg: CFG): (TestGenerator, PathCoverage) = {
    val paths = cfg.forwardPathBuilderAux[None.type](0, Vector(0), i, cfg.emptyLoopStates, None, (_, _, _) => (None, None)).toVector
    (
      TestGeneratorMap(cfg, "path_all", All, paths.map(path => path.toString -> PathTestGenerator(cfg, "leaf", path)).toMap),
      PathCoverage(paths.toSet.map(Path))
    )
  }
}

case class AllDefinitionsCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all definitions"

  override def build(cfg: CFG): (TestGenerator, SourceTargetCoverage) = {
    val pathsChunks: Map[Int, Set[Vector[Int]]] = cfg.labels
      .filterKeys(cfg.isAssign)
      .map {
        case (label: Int, AST.Assign(tVariable, _)) =>
          label -> Criterion.firstRefFromDefPaths(cfg, label, tVariable, Vector(label), maxLoopDepth)
      }

    val extendedPathsChunks: Map[String, Map[String, Vector[Vector[Int]]]] = pathsChunks.map {
      case (defId, chunks) => defId.toString -> chunks.map(chunk =>
        chunk(0).toString + "~>" + chunk.last.toString -> cfg.fillPathUp(chunk, maxLoopDepth)
      ).toMap
    }
    (
      TestGeneratorMap(cfg, "definition usage", All, extendedPathsChunks.map {
        case (k, definitionPaths) => "def_" + k -> TestGeneratorMap(cfg, "path", Any, definitionPaths.map {
          case (chunkDef, definitionPath) => chunkDef -> TestGeneratorMap(cfg, "path", Any, definitionPath.zipWithIndex.map {
            case (path, m) => m.toString -> PathTestGenerator(cfg, "leaf", path)
          }.toMap)
        })
      }),
      SourceTargetCoverage(pathsChunks.map { case
        (source, chunkSet) =>
        if (chunkSet.nonEmpty){
          val pick = chunkSet.head
          SourceTarget(Some(source), Some(pick.last))
        } else {
          SourceTarget(Some(source), None)
        }
      }.toSet)
    )
  }
}

case class AllUsagesCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all usages"

  override def build(cfg: CFG): (TestGenerator, SourceTargetCoverage) = {
    val du_paths: Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = Criterion.allRefToDefPaths(cfg, maxLoopDepth)
    val extended_du_paths: Map[AST.A.Expression.Variable, Map[Int, Map[String, Vector[Vector[Int]]]]] =
      du_paths.mapValues(var_du_paths =>
        var_du_paths.mapValues(chunks =>
          chunks.map(chunk =>
            chunk.mkString("~>") -> cfg.fillPathUp(chunk, maxLoopDepth)
          ).toMap
        )
      )

    (
      TestGeneratorMap(cfg, "all usages", All, extended_du_paths.map {
        case (key, map1) => key.toString -> TestGeneratorMap(cfg, "all_ref", All, map1.map {
          case (refId, map2) => refId.toString -> TestGeneratorMap(cfg, "all_def_to_ref", Any, map2.map {
            case (defToRef, map3) => defToRef -> TestGeneratorMap(cfg, "any_path", Any, map3.zipWithIndex.map {
              case (path, idx) => idx.toString -> PathTestGenerator(cfg, "leaf", path)
            }.toMap)
          })
        })
      }),
      SourceTargetCoverage(du_paths.flatMap {
        case (_, var_paths) => var_paths.map {
          case (_, set) =>
            val pick = set.head
            SourceTarget(Some(pick(0)), Some(pick.last))
        }
      }.toSet)
    )
  }
}

case class AllDUPathsCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all du paths"

  override def build(cfg: CFG): (TestGenerator, PathCoverage) = {
    val du_paths: Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = Criterion.allRefToDefPaths(cfg, maxLoopDepth)
    val extended_du_paths: Map[AST.A.Expression.Variable, Map[Int, Map[String, Vector[Vector[Int]]]]] =
      du_paths.mapValues(var_du_paths =>
        var_du_paths.mapValues(chunks =>
          chunks.map(chunk =>
            chunk.mkString("~>") -> cfg.fillPathUp(chunk, maxLoopDepth)
          ).toMap
        )
      )

    (
      TestGeneratorMap(cfg, "all du paths", All, extended_du_paths.map {
        case (key, map1) => key.toString -> TestGeneratorMap(cfg, "ref", All, map1.map {
          case (refId, map2) => refId.toString -> TestGeneratorMap(cfg, "def_to_ref", All, map2.map {
            case (defToRef, map3) => defToRef -> TestGeneratorMap(cfg, "path", Any, map3.zipWithIndex.map {
              case (path, idx) => idx.toString -> PathTestGenerator(cfg, "leaf", path)
            }.toMap)
          })
        })
      }),
      PathCoverage(du_paths.flatMap {
        case (_, m) => m.flatMap {
          case (_, set) => set
        }
      }.toSet.map(Path))
    )
  }
}

object Criterion {

  def criterionFromRequiredNodes(cfg: CFG, maxLoopExec: Int, requiredNodes: Set[Int]): TestGenerator = {

    val requiredPathsForNodes: Map[String, Vector[Vector[Int]]] =
      requiredNodes
        .map((a: Int) => a.toString -> Vector(a))
        .toMap
        .mapValues(cfg.fillPathUp(_, maxLoopExec))

    TestGeneratorMap(cfg, "node", All,
      requiredPathsForNodes.mapValues(pathSet =>
        TestGeneratorMap(cfg, "path", Any, pathSet.zipWithIndex.map {
          case (a, b) => b.toString -> PathTestGenerator(cfg, "constraint", a)
        }.toMap)
      )
    )
  }

  def firstRefFromDefPaths(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
    def lambda(nextNodeLabel: Int, localVariable: AST.A.Expression.Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST.A.Expression.Variable) = {
      val cmd = cfg.getAST(nextNodeLabel)
      if (!cfg.labels.contains(nextNodeLabel)) (Some(Set[Vector[Int]]()), localVariable)
      else if (cmd.isRef(localVariable)) (Some(Set(path :+ nextNodeLabel)), localVariable)
      else if (cmd.isDef(localVariable)) (Some(Set[Vector[Int]]()), localVariable)
      else (None, variable)
    }

    val loopStates = cfg.labels.keys.filter(cfg.isWhile).map(_ -> 0).toMap
    cfg.forwardPathBuilderAux(label, Vector(label), maxLoopExec, loopStates, variable, lambda)
  }

  def firstDefFromRefPaths(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
    def lambda(nextNodeLabel: Int, localVariable: AST.A.Expression.Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST.A.Expression.Variable) = {
      val cmd = cfg.getAST(nextNodeLabel)
      if (cmd.isDef(variable)) (Some(Set(nextNodeLabel +: path)), localVariable)
      else if (nextNodeLabel == 0) (Some(Set[Vector[Int]]()), localVariable)
      else (None, localVariable)
    }

    val loopStates = cfg.labels.keys.filter(cfg.isWhile).map(_ -> 0).toMap
    cfg.backwardPathBuilderAux(label, Vector(label), maxLoopExec, loopStates, variable, lambda).filter(_.length > 1)
  }

  def allRefToDefPaths(cfg: CFG, maxLoopDepth: Int): Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = {
    val variables =
      cfg.labels.values
        .filter {
          case AST.Assign(_, _) => true
          case _ => false
        }.map {
        case AST.Assign(a, _) => a
      }
        .toSet

    val refs =
      variables.map { variable =>
        variable ->
          cfg.labels
            .filter {
              case (_, cmd) => cmd.isRef(variable)
            }.map {
            case (lbl, _) => lbl
          }
      }.toMap

    val usagePaths = refs.map {
      case (tVar, potentialRefs) =>
        tVar -> potentialRefs
          .map { potentialRef =>
            potentialRef -> firstDefFromRefPaths(cfg, potentialRef, tVar, Vector(potentialRef), maxLoopDepth)
          }.toMap.filter {
          case (_, set) => set.nonEmpty
        }
    }

    usagePaths
  }

}

sealed abstract class CoverageUnit

case class Path(path: Vector[Int]) extends CoverageUnit {
  override def toString: String = {path.mkString("~>")}

  def contains(path2: Path): Boolean = path.containsSlice(path2.path)
}

case class SourceTarget(source: Option[Int], target: Option[Int]) extends CoverageUnit {
  override def toString: String = source.map(_.toString).getOrElse("") + "~~>" + target.map(_.toString).getOrElse("")
}

case class Node(node: Int) extends CoverageUnit {
  override def toString: String = node.toString
}

sealed abstract class Coverage {

  def coverTest(cfg: CFG, states: List[State]): Option[CoverageUnit]

  def coverTestString(cfg: CFG, states: List[State]): String
}

case class PathCoverage(paths: Set[Path]) extends Coverage {
  override def coverTest(cfg: CFG, states: List[State]): Option[Path] = {
    val traversedPaths = states.map(state =>
      Path((Vector[Int]() /: cfg.exec(0, state)) {
        case (accV, node) => accV :+ node.value
      })
    )

    paths
      .map(path =>
        traversedPaths
          .find(_.contains(path)) match {
          case Some(_) => None
          case None => Some(path)
        })
      .find(_.nonEmpty)
      .flatten
  }

  override def coverTestString(cfg: CFG, states: List[State]): String =
    this.coverTest(cfg, states) match {
      case Some(path) => s"path $path not traversed"
      case None => "all required paths traversed"
    }

  override def toString: String = s"{${paths.mkString(", ")}}"
}

case class SourceTargetCoverage(st: Set[SourceTarget]) extends Coverage {
  override def coverTest(cfg: CFG, states: List[State]): Option[SourceTarget] = {
    val traversedPaths = states.map(state =>
      Path((Vector[Int]() /: cfg.exec(0, state)) {
        case (accV, node) => accV :+ node.value
      })
    )

    st.map{
      case p @ SourceTarget(Some(source), Some(target))=>
        traversedPaths.find(path=> {
        val sourceIndexInPath = path.path.indexOf(source)
        if(sourceIndexInPath >= 0) {
          val targetIndexInPath = path.path.indexOf(target, sourceIndexInPath + 1)
          targetIndexInPath >= 0
        }
        else false
      }) match {
        case Some(_) => None
        case None => Some(p)
      }
      case p => Some(p)
    }
      .find(_.nonEmpty)
      .flatten
  }

  override def coverTestString(cfg: CFG, states: List[State]): String =
    this.coverTest(cfg, states) match {
      case Some(SourceTarget(Some(source), Some(target))) => s"path going from $source to $target not traversed"
      case Some(SourceTarget(Some(source), None)) => s"no target associated to source $source"
      case None => s"all required source target paths found"
    }
}

case class NodeCoverage(nodes: Set[Node]) extends Coverage {
  override def coverTest(cfg: CFG, states: List[State]): Option[Node] = {
    val traversedPaths = states.map(state =>
      (Vector[Node]() /: cfg.exec(0, state)) {
        case (accV, node) => accV :+ Node(node.value)
      }
    )

    val traversedNodes = traversedPaths.flatten

    nodes
      .map(
        node =>
          if (traversedNodes.contains(node)) None
          else Some(node)
      )
      .find(_.nonEmpty)
      .flatten
  }

  override def toString: String = s"nodes : {${nodes.mkString(", ")}}"

  override def coverTestString(cfg: CFG, states: List[State]): String =
    this.coverTest(cfg, states) match {
      case Some(n) => s"node ${n.toString} not traversed"
      case None => s"all required nodes traversed"
    }
}
