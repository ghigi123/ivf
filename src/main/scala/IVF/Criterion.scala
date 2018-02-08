package IVF

object Criterion {

  def criterionFromRequiredNodes(cfg: CFG, maxLoopExec: Int, requiredNodes: Set[Int]): TestGenerator = {

    println(requiredNodes)
    val requiredPathsForNodes: Map[String, Vector[Vector[Int]]] =
      requiredNodes
        .map((a: Int) => a.toString -> Vector(a))
        .toMap
        .mapValues(cfg.fillPathUp(_, maxLoopExec))

    println(requiredPathsForNodes)

    TestGeneratorMap(cfg, "node_all", All,
      requiredPathsForNodes.mapValues(pathSet =>
        TestGeneratorMap(cfg, "path_any", Any, pathSet.zipWithIndex.map {
          case (a, b) => b.toString -> PathTestGenerator(cfg, "leaf", a)
        }.toMap)
      )
    )
  }

  def allAssignCriterion(cfg: CFG, maxLoopExec: Int): (TestGenerator, Coverage) = {
    val requiredNodes = cfg.labels.filterKeys(cfg.isAssign).keys.toSet

    (
      criterionFromRequiredNodes(cfg, maxLoopExec, requiredNodes),
      NodeCoverage(requiredNodes)
    )
  }

  def allDecisionsCriterion(cfg: CFG, maxLoopExec: Int): (TestGenerator, Coverage) = {
    val requiredNodes: Set[Int] = cfg.graph.edges
      .filter(e => e.label == "true" || e.label == "false")
      .map(e => e.to.value)
      .toSet

    (
      criterionFromRequiredNodes(cfg, maxLoopExec, requiredNodes),
      NodeCoverage(requiredNodes)
    )
  }

  def allKPathsCriterion(cfg: CFG, k: Int): (TestGenerator, Coverage) = {
    def lambda(nextNodeLabel: Int, localK: Int, path: Vector[Int]): (Option[Set[Vector[Int]]], Int) =
      if (localK == 0)
        (Some(Set[Vector[Int]]()), localK)
      else
        (None, localK - 1)

    val paths: Seq[Vector[Int]] = cfg.forwardPathBuilderAux(0, Vector(0), Int.MaxValue, cfg.emptyLoopStates, k, lambda).toVector

    (
      TestGeneratorMap(cfg, "path_all", All, paths.map(path => path.toString -> PathTestGenerator(cfg, "leaf", path)).toMap),
      PathCoverage(paths.toSet)
    )
  }

  def allILoopsCriterion(cfg: CFG, i: Int): (TestGenerator, Coverage) = {
    val paths = cfg.forwardPathBuilderAux[None.type](0, Vector(0), i, cfg.emptyLoopStates, None, (_, _, _) => (None, None)).toVector
    (
      TestGeneratorMap(cfg, "path_all", All, paths.map(path => path.toString -> PathTestGenerator(cfg, "leaf", path)).toMap),
      PathCoverage(paths.toSet)
    )
  }

  private def firstRefFromDefPaths(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
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


  def allDefinitionsCriterion(cfg: CFG, maxLoopExec: Int): (TestGenerator, Coverage) = {
    val pathsChunks: Map[Int, Set[Vector[Int]]] = cfg.labels
      .filterKeys(cfg.isAssign)
      .map {
        case (label: Int, AST.Assign(tVariable, _)) =>
          label -> firstRefFromDefPaths(cfg, label, tVariable, Vector(label), maxLoopExec)
      }

    val extendedPathsChunks: Map[String, Map[String, Vector[Vector[Int]]]] = pathsChunks.map {
      case (defId, chunks) => defId.toString -> chunks.map(chunk =>
        chunk(0).toString + "~>" + chunk.last.toString -> cfg.fillPathUp(chunk, maxLoopExec)
      ).toMap
    }
    (
      TestGeneratorMap(cfg, "definition", All, extendedPathsChunks.map {
        case (k, definitionPaths) => "def_" + k -> TestGeneratorMap(cfg, "path", Any, definitionPaths.map {
          case (chunkDef, definitionPath) => chunkDef -> TestGeneratorMap(cfg, "path", Any, definitionPath.zipWithIndex.map {
            case (path, m) => m.toString -> PathTestGenerator(cfg, "leaf", path)
          }.toMap)
        })
      }),
      SourceTargetCoverage(pathsChunks.map { case
        (_, chunkSet) =>
        val pick = chunkSet.head
        Vector(pick(0), pick.last)
      }.toSet)
    )
  }


  def firstDefFromRefPaths(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
    def lambda(nextNodeLabel: Int, localVariable: AST.A.Expression.Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST.A.Expression.Variable) = {
      val cmd = cfg.getAST(nextNodeLabel)
      if (cmd.isDef(variable)) (Some(Set(nextNodeLabel +: path)), localVariable)
      else if (nextNodeLabel == 0) (Some(Set[Vector[Int]]()), localVariable)
      else (None, localVariable)
    }

    val loopStates = cfg.labels.keys.filter(cfg.isWhile).map(_ -> 0).toMap
    cfg.backwardPathBuilderAux(label, Vector(label), maxLoopExec, loopStates, variable, lambda)
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

  def allUsagesCriterion(cfg: CFG, maxLoopDepth: Int): (TestGenerator, Coverage) = {
    val du_paths: Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = allRefToDefPaths(cfg, maxLoopDepth)
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
            Vector(pick(0), pick.last)
        }
      }.toSet)
    )
  }

  def allDUPathsCriterion(cfg: CFG, maxLoopDepth: Int): (TestGenerator, Coverage) = {
    val du_paths: Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = allRefToDefPaths(cfg, maxLoopDepth)
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
      }.toSet)
    )
  }
}

sealed abstract class Coverage

case class PathCoverage(paths: Set[Vector[Int]]) extends Coverage

case class SourceTargetCoverage(st: Set[Vector[Int]]) extends Coverage

case class NodeCoverage(nodes: Set[Int]) extends Coverage
