package IVF.Criterion

import IVF.Model.CFG
import IVF.TestGenerator._
import IVF.Model._

/**
  * Represents a criterion
  */
sealed abstract class Criterion {
  /**
    * Criterion name
    *
    * @return
    */
  def name: String

  /**
    * Returns a test generator to generate state set from this criterion and a coverage to test this criterion on an already existing test set
    *
    * @param cfg control flow graph
    * @return
    */
  def build(cfg: CFG): (TestGenerator, Coverage)
}

/**
  * All assigns criterion
  *
  * @param maxLoopDepth max loop execution during test generation
  */
case class AllAssignCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all assigns"

  override def build(cfg: CFG): (TestGenerator, NodeCoverage) = {
    // just get the required nodes with a simple filter on assignments
    val requiredNodes = cfg.labels.filterKeys(cfg.isAssign).keys.toSet
    (
      Criterion.testGeneratorFromRequiredNodes(cfg, maxLoopDepth, requiredNodes),
      NodeCoverage(requiredNodes.map(Node))
    )
  }
}

/**
  * All decisions criterion
  * @param maxLoopDepth max loop execution during test generation
  */
case class AllDecisionsCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all decisions"

  override def build(cfg: CFG): (TestGenerator, NodeCoverage) = {
    // a simple filter is enough here too
    val requiredNodes: Set[Int] = cfg.graph.edges
      .filter(e => e.label == "true" || e.label == "false")
      .map(e => e.to.value)
      .toSet

    (
      Criterion.testGeneratorFromRequiredNodes(cfg, maxLoopDepth, requiredNodes),
      NodeCoverage(requiredNodes.map(Node))
    )
  }
}

/**
  * All k-paths criterion
  * @param k k
  */
case class AllKPathCriterion(k: Int) extends Criterion {
  override def name: String = s"all k paths with k=${k.toString}"

  override def build(cfg: CFG): (TestGenerator, PathCoverage) = {
    // the lambda is here quite simple : decrease k until is is 0, and if we reach 0, cancel
    // note the infinity on the number of i loops allowed
    def lambda(nextNodeLabel: Int, localK: Int, path: Vector[Int]): (Option[Set[Vector[Int]]], Int) =
      if (localK == 0)
        (Some(Set[Vector[Int]]()), localK)
      else
        (None, localK - 1)

    val paths: Seq[Vector[Int]] = cfg.forwardPathBuilderAux(0, Vector(0), Int.MaxValue, cfg.emptyLoopStates, k, lambda).toVector

    // more in report
    (
      TestGeneratorMap(cfg, "path", All, paths.map(path => Path(path).toString -> PathTestGenerator(cfg, "constraint", path)).toMap),
      PathCoverage(paths.toSet.map(Path))
    )
  }
}

/**
  * All i-loops criterion
  * @param i i
  */
case class AllILoopsCriterion(i: Int) extends Criterion {
  override def name: String = s"all i loops with i=${i.toString}"

  override def build(cfg: CFG): (TestGenerator, PathCoverage) = {
    // the forwardPathBuilderAux with empty lambda is exactly the i loop criterion
    val paths = cfg.forwardPathBuilderAux[None.type](0, Vector(0), i, cfg.emptyLoopStates, None, (_, _, _) => (None, None)).toVector
    // more in report
    (
      TestGeneratorMap(cfg, "path_all", All, paths.map(path => path.toString -> PathTestGenerator(cfg, "leaf", path)).toMap),
      PathCoverage(paths.toSet.map(Path))
    )
  }
}

/**
  * All definitions criterion
  * @param maxLoopDepth max loop execution during test generation
  */
case class AllDefinitionsCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all definitions"

  override def build(cfg: CFG): (TestGenerator, SourceAnyTargetCoverage) = {
    // here we get the paths from a definition to its usages
    val pathsChunks: Map[Int, Set[Vector[Int]]] = cfg.labels
      .filterKeys(cfg.isAssign)
      .map {
        case (label: Int, AST.Assign(tVariable, _)) =>
          label -> Criterion.firstRefFromDefPaths(cfg, label, tVariable, maxLoopDepth)
      }

    // then we extend those paths up
    val extendedPathsChunks: Map[String, Map[String, Vector[Vector[Int]]]] = pathsChunks.map {
      case (defId, chunks) => defId.toString -> chunks.map(chunk =>
        chunk(0).toString + "~>" + chunk.last.toString -> cfg.fillPathUp(chunk, maxLoopDepth)
      ).toMap
    }

    // and write the criterion (look at the report)
    (
      TestGeneratorMap(cfg, "definition usage", All, extendedPathsChunks.map {
        case (k, definitionPaths) => "def_" + k -> TestGeneratorMap(cfg, "path", Any, definitionPaths.map {
          case (chunkDef, definitionPath) => chunkDef -> TestGeneratorMap(cfg, "path", Any, definitionPath.zipWithIndex.map {
            case (path, m) => m.toString -> PathTestGenerator(cfg, "leaf", path)
          }.toMap)
        })
      }),
      SourceAnyTargetCoverage(pathsChunks.map {
        case (source, chunkSet) =>
          SourceTargets(Some(source), chunkSet.map(_.last))
      }.toSet)
    )
  }
}

/**
  * All usages criterion
  *
  * @param maxLoopDepth max loop execution during test generation
  */
case class AllUsagesCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all usages"

  override def build(cfg: CFG): (TestGenerator, SourceTargetCoverage) = {
    // just get the ref <-> def tuple possible paths
    val du_paths: Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = Criterion.allRefToDefPaths(cfg, maxLoopDepth)
    // then group the paths by def <-> ref so that the Any test generator matches the usages and not all the def <-> ref paths
    // (this way just one usage per definition in required : before this operation, if we had 1~>2~>4 path, it was not considered
    // as the same usage as 1~>3~>4, we just merge this to a 1~~>4)
    val grouped_du_paths = du_paths.mapValues(_.mapValues(_.groupBy(vec => vec(0) + "~~>" + vec.last)))
    // extend this path collection up
    val extended_du_paths: Map[AST.A.Expression.Variable, Map[Int, Map[String, Map[String, Vector[Vector[Int]]]]]] =
      grouped_du_paths.mapValues(var_du_paths =>
        var_du_paths.mapValues(ref_du_paths =>
          ref_du_paths.mapValues(chunks =>
            chunks.map(chunk =>
              chunk.mkString("~>") -> cfg.fillPathUp(chunk, maxLoopDepth)
            ).toMap
          )
        )
      )

    // look at the report for more about this
    (
      TestGeneratorMap(cfg, "variable", All, extended_du_paths.map {
        case (key, map1) => key.toString -> TestGeneratorMap(cfg, "ref", All, map1.map {
          case (refId, map2) => refId.toString -> TestGeneratorMap(cfg, "def to ref", All, map2.map {
            case (defToRefId, map4) => defToRefId.toString -> TestGeneratorMap(cfg, "def to ref path", Any, map4.map {
              case (defToRef, map3) => defToRef -> TestGeneratorMap(cfg, "extended def to ref path", Any, map3.zipWithIndex.map {
                case (path, idx) => idx.toString -> PathTestGenerator(cfg, "constraint", path)
              }.toMap)
            })
          })
        })
      }),
      SourceTargetCoverage(du_paths.flatMap {
        case (_, variableMap) => variableMap.flatMap {
          case
            (target, chunkSet) =>
            chunkSet.map(pick => SourceTarget(Some(pick(0)), Some(target)))
        }
      }.toSet)
    )
  }
}

/**
  * All DU paths criterion
  *
  * @param maxLoopDepth max loop execution during test generation
  */
case class AllDUPathsCriterion(maxLoopDepth: Int = 1) extends Criterion {
  override def name: String = "all du paths"

  override def build(cfg: CFG): (TestGenerator, PathCoverage) = {
    // just get the ref <-> def tuple possible paths
    val du_paths: Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = Criterion.allRefToDefPaths(cfg, maxLoopDepth)
    // build all the paths up until the beginning of the graph is found
    val extended_du_paths: Map[AST.A.Expression.Variable, Map[Int, Map[String, Vector[Vector[Int]]]]] =
      du_paths.mapValues(var_du_paths =>
        var_du_paths.mapValues(chunks =>
          chunks.map(chunk =>
            chunk.mkString("~>") -> cfg.fillPathUp(chunk, maxLoopDepth)
          ).toMap
        )
      )
    // see report for more about this structure
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

/**
  * Utils to build criterions
  */
object Criterion {

  /**
    * Returns a test generator associated to a list of nodes
    *
    * @param cfg           control flow graph
    * @param maxLoopExec   max loop execution during test generation
    * @param requiredNodes required nodes
    * @return
    */
  def testGeneratorFromRequiredNodes(cfg: CFG, maxLoopExec: Int, requiredNodes: Set[Int]): TestGenerator = {

    val requiredPathsForNodes: Map[String, Vector[Vector[Int]]] =
      requiredNodes
        .map((a: Int) => a.toString -> Vector(a))
        .toMap
        // here we just build in the down-top direction all the path from the required nodes
        .mapValues(cfg.fillPathUp(_, maxLoopExec))

    // see report for more about this
    TestGeneratorMap(cfg, "node", All,
      requiredPathsForNodes.mapValues(pathSet =>
        TestGeneratorMap(cfg, "path", Any, pathSet.zipWithIndex.map {
          case (a, b) => b.toString -> PathTestGenerator(cfg, "constraint", a)
        }.toMap)
      )
    )
  }

  /**
    * Returns paths to the first reference from a given definition (going down)
    *
    * @param cfg         control flow graph
    * @param label       start label (reference)
    * @param variable    defined variable
    * @param maxLoopExec max loop execution during test generation
    * @return
    */
  def firstRefFromDefPaths(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, maxLoopExec: Int): Set[Vector[Int]] = {
    // lamda defines the stop condition for forwardPathBuilderAux
    def lambda(nextNodeLabel: Int, localVariable: AST.A.Expression.Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST.A.Expression.Variable) = {
      val cmd = cfg.getAST(nextNodeLabel)
      if (!cfg.labels.contains(nextNodeLabel)) (Some(Set[Vector[Int]]()), localVariable)
      // we stop when we find a reference and send the traversed path back
      else if (cmd.isRef(localVariable)) (Some(Set(path :+ nextNodeLabel)), localVariable)
      // if there is another definition of the same variable, it means that we need to stop here, and return that we have not found a path
      else if (cmd.isDef(localVariable)) (Some(Set[Vector[Int]]()), localVariable)
      else (None, variable)
    }

    val loopStates = cfg.labels.keys.filter(cfg.isWhile).map(_ -> 0).toMap
    cfg.forwardPathBuilderAux(label, Vector(label), maxLoopExec, loopStates, variable, lambda) // look at doc
  }

  /**
    * Returns paths to the first definition from a given reference (going up)
    *
    * @param cfg         control flow graph
    * @param label       start point (reference)
    * @param variable    referenced variable
    * @param maxLoopExec max loop execution during test generation
    * @return
    */
  def firstDefFromRefPaths(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, maxLoopExec: Int): Set[Vector[Int]] = {
    def lambda(nextNodeLabel: Int, localVariable: AST.A.Expression.Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST.A.Expression.Variable) = {
      val cmd = cfg.getAST(nextNodeLabel)
      if (cmd.isDef(variable)) (Some(Set(nextNodeLabel +: path)), localVariable)
      else if (nextNodeLabel == 0) (Some(Set[Vector[Int]]()), localVariable)
      else (None, localVariable)
    }

    val loopStates = cfg.labels.keys.filter(cfg.isWhile).map(_ -> 0).toMap
    cfg.backwardPathBuilderAux(label, Vector(label), maxLoopExec, loopStates, variable, lambda).filter(_.length > 1)
  }

  /**
    * Extends [[firstDefFromRefPaths]] enabling to get all reference to definition paths
    *
    * @param cfg          control flow graph
    * @param maxLoopDepth max loop execution during test generation
    * @return map of variables to a map of usages to a set of paths going up from the reference until the matching definition is found
    */
  def allRefToDefPaths(cfg: CFG, maxLoopDepth: Int): Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = {
    // we will return the result organized by variable
    val variables =
      cfg.labels.values
        .filter {
          case AST.Assign(_, _) => true
          case _ => false
        }.map {
        case AST.Assign(a, _) => a
      }
        .toSet

    // each variable will actually be associated a reference list
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

    // every reference will be associated a definition reference, and, all paths going from def to ref
    val usagePaths = refs.map {
      case (tVar, potentialRefs) =>
        tVar -> potentialRefs
          .map { potentialRef =>
            potentialRef -> firstDefFromRefPaths(cfg, potentialRef, tVar, maxLoopDepth)
          }.toMap.filter {
          case (_, set) => set.nonEmpty
        }
    }

    // return this huge map
    usagePaths
  }

}

