package IVF

import IVF.AST.A.Expression
import IVF.main.Utils
import org.chocosolver.solver.Model
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.variables.IntVar

object Criterion {
  type Path = Vector[Int]
}

sealed trait Required

case object All extends Required

case object Any extends Required

sealed trait GenTest {
  def valid: Boolean

  def checkError: Option[String]

  def flatten: Set[State]
}

case class GenTestState(state: State, criterion: Criterion) extends GenTest {
  override def valid: Boolean = true

  override def checkError: Option[String] = None

  override def flatten: Set[State] = Set(state)
}

case class GenTestError(error: String, criterion: Criterion) extends GenTest {
  override def valid: Boolean = false

  override def checkError: Option[String] = Some(criterion.name + " -> " + error)

  override def flatten: Set[State] = Set()
}

case class GenTestMap(map: Map[String, GenTest], required: Required, criterion: Criterion) extends GenTest {
  override def valid: Boolean = required match {
    case All => map.forall {
      case (_, t) => t.valid
    }
    case Any => map.exists {
      case (_, t) => t.valid
    }
  }

  override def checkError: Option[String] = required match {
    case All =>
      map.mapValues(_.checkError).find {
        case (_, Some(_)) => true
        case (_, None) => false
      } match {
        case Some((k, (Some(err)))) => Some("Missing " + criterion.name + ": " + k + " -> " + err)
        case _ => None
      }
    case Any =>
      if (map.values.map(_.checkError).exists {
        case Some(_) => false
        case None => true
      })
        None
      else Some("Unable to find any " + criterion.name)
  }

  override def flatten: Set[State] = required match {
    case All => map.values.flatMap(_.flatten).toSet
    case Any => Set(map.values.flatMap(_.flatten).head)
  }

}


abstract class Criterion(val cfg: CFG, val name: String) {
  def generateTests(): GenTest

  override def toString: String = name + ": "
}

case class CriterionMap(override val cfg: CFG, override val name: String, required: Required, criteria: Map[String, Criterion]) extends Criterion(cfg, name) {

  override def generateTests(): GenTest = required match {
    case All =>
      GenTestMap(criteria.map {
        case (key, crit) => key -> crit.generateTests()
      }, All, this)
    case Any => criteria.find {
      case (_, crit) => crit.generateTests().valid
    } match {
      case Some((key, c)) => GenTestMap(Map(key -> c.generateTests()), Any, this)
      case None => GenTestMap(Map(), Any, this)
    }
  }

  override def toString: String = super.toString + criteria.map {
    case (key, value) => key + "->" + value.toString
  }.mkString("\n")
}

case class PathCriterion(override val cfg: CFG, override val name: String, path: Criterion.Path) extends Criterion(cfg, name) {
  def generateTests(): GenTest = {
    val constraints = Utils.BuildConstraints(cfg, path)
    val model: Model = new Model()
    val variables = (Map[String, IntVar]() /: constraints) {
      case (accMap, exp) =>
        (accMap /: Utils.identifyVariables(exp)) {
          case (accMap2, tVar) => if (accMap.contains(tVar.tName))
            accMap2
          else
            accMap2 + (tVar.tName -> model.intVar(tVar.tName, -100, 100))
        }
    }

    constraints.foreach((exp: AST.B.Expression) => {
      val constraint = exp.toConstraintVar(model, variables).eq(model.boolVar(true))
      constraint.post()
    })

    val solver = model.getSolver

    solver.setSearch(Search.minDomLBSearch(variables.values.toSeq: _*))
    if (solver.solve()) {
      GenTestState(State(variables
        .filterKeys(!_.contains('_'))
        .mapValues(_.getValue)
      ), this)
    } else {
      GenTestError("Unable to find valid valuation for path " + path.toString, this)
    }
  }

  override def toString: String = "P" + path.map(_.toString).mkString("_")

}

object CriterionUtils {

  def criterionFromRequiredNodes(cfg: CFG, maxLoopExec: Int, requiredNodes: Set[Int]): Criterion = {

    println(requiredNodes)
    val requiredPathsForNodes: Map[String, Vector[Vector[Int]]] =
      requiredNodes
        .map((a: Int) => a.toString -> Vector(a))
        .toMap
        .mapValues(cfg.fillPathUp(_, maxLoopExec))

    println(requiredPathsForNodes)

    CriterionMap(cfg, "node_all", All,
      requiredPathsForNodes.mapValues(pathSet =>
        CriterionMap(cfg, "path_any", Any, pathSet.zipWithIndex.map {
          case (a, b) => b.toString -> PathCriterion(cfg, "leaf", a)
        }.toMap)
      )
    )
  }

  def allAssignCriterion(cfg: CFG, maxLoopExec: Int): Criterion = {
    val requiredNodes = cfg.labels.filterKeys(cfg.isAssign).keys.toSet

    criterionFromRequiredNodes(cfg, maxLoopExec, requiredNodes)
  }

  def allDecisionsCriterion(cfg: CFG, maxLoopExec: Int): Criterion = {
    val requiredNodes: Set[Int] = cfg.graph.edges
      .filter(e => e.label == "true" || e.label == "false")
      .map(e => e.to.value)
      .toSet

    criterionFromRequiredNodes(cfg, maxLoopExec, requiredNodes)
  }

  def allKPathsCriterion(cfg: CFG, k: Int): Criterion = {
    def lambda(nextNodeLabel: Int, localK: Int, path: Vector[Int]): (Option[Set[Vector[Int]]], Int) =
      if (localK == 0)
        (Some(Set[Vector[Int]]()), localK)
      else
        (None, localK - 1)

    val paths = cfg.forwardPathBuilderAux(0, Vector(0), Int.MaxValue, cfg.emptyLoopStates, k, lambda).toVector

    CriterionMap(cfg, "path_all", All, paths.map(path => path.toString -> PathCriterion(cfg, "leaf", path)).toMap)
  }

  def allILoopsCriterion(cfg: CFG, i: Int): Criterion = {
    val paths = cfg.forwardPathBuilderAux[None.type](0, Vector(0), i, cfg.emptyLoopStates, None, (_, _, _) => (None, None)).toVector
    println(paths)
    CriterionMap(cfg, "path_all", All, paths.map(path => path.toString -> PathCriterion(cfg, "leaf", path)).toMap)
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


  def allDefinitionsCriterion(cfg: CFG, maxLoopExec: Int): Criterion = {
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

    CriterionMap(cfg, "definition", All, extendedPathsChunks.map {
      case (k, definitionPaths) => "def_" + k -> CriterionMap(cfg, "path", Any, definitionPaths.map {
        case (chunkDef, definitionPath) => chunkDef -> CriterionMap(cfg, "path", Any, definitionPath.zipWithIndex.map {
          case (path, m) => m.toString -> PathCriterion(cfg, "leaf", path)
        }.toMap)
      })
    })
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

  def allUsagesCriterion(cfg: CFG, maxLoopDepth: Int): Criterion = {
    val du_paths: Map[Expression.Variable, Map[Int, Set[Vector[Int]]]] = allRefToDefPaths(cfg, maxLoopDepth)
    val extended_du_paths: Map[Expression.Variable, Map[Int, Map[String, Vector[Vector[Int]]]]] =
      du_paths.mapValues(var_du_paths =>
        var_du_paths.mapValues(chunks =>
          chunks.map(chunk =>
            chunk.mkString("~>") -> cfg.fillPathUp(chunk, maxLoopDepth)
          ).toMap
        )
      )

    CriterionMap(cfg, "all usages", All, extended_du_paths.map {
      case (key, map1) => key.toString -> CriterionMap(cfg, "all_ref", All, map1.map {
        case (refId, map2) => refId.toString -> CriterionMap(cfg, "all_def_to_ref", Any, map2.map {
          case (defToRef, map3) => defToRef -> CriterionMap(cfg, "any_path", Any, map3.zipWithIndex.map {
            case (path, idx) => idx.toString -> PathCriterion(cfg, "leaf", path)
          }.toMap)
        })
      })
    })
  }

  def allDUPathsCriterion(cfg: CFG, maxLoopDepth: Int): Criterion = {
    val du_paths: Map[Expression.Variable, Map[Int, Set[Vector[Int]]]] = allRefToDefPaths(cfg, maxLoopDepth)
    val extended_du_paths: Map[Expression.Variable, Map[Int, Map[String, Vector[Vector[Int]]]]] =
      du_paths.mapValues(var_du_paths =>
        var_du_paths.mapValues(chunks =>
          chunks.map(chunk =>
            chunk.mkString("~>") -> cfg.fillPathUp(chunk, maxLoopDepth)
          ).toMap
        )
      )

    CriterionMap(cfg, "all du paths", All, extended_du_paths.map {
      case (key, map1) => key.toString -> CriterionMap(cfg, "ref", All, map1.map {
        case (refId, map2) => refId.toString -> CriterionMap(cfg, "def_to_ref", All, map2.map {
          case (defToRef, map3) => defToRef -> CriterionMap(cfg, "path", Any, map3.zipWithIndex.map {
            case (path, idx) => idx.toString -> PathCriterion(cfg, "leaf", path)
          }.toMap)
        })
      })
    })
  }
}
