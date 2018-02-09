package IVF

import org.chocosolver.solver.Model
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.variables.IntVar

object TestGenerator {
  type Path = Vector[Int]
}

sealed trait Required

case object All extends Required

case object Any extends Required

sealed trait GeneratedTest {
  def valid: Boolean

  def checkError: Option[String]

  def flatten: Set[State]
}

case class GeneratedTestState(state: State, criterion: TestGenerator) extends GeneratedTest {
  override def valid: Boolean = true

  override def checkError: Option[String] = None

  override def flatten: Set[State] = Set(state)
}

case class GeneratedTestError(error: String, criterion: TestGenerator) extends GeneratedTest {
  override def valid: Boolean = false

  override def checkError: Option[String] = Some(criterion.name + " -> " + error)

  override def flatten: Set[State] = Set()
}

case class GeneratedTestMap(map: Map[String, GeneratedTest], required: Required, criterion: TestGenerator) extends GeneratedTest {
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


sealed abstract class TestGenerator(val cfg: CFG, val name: String) {
  def generateTests(): GeneratedTest

  override def toString: String = name + ": "
}

case class TestGeneratorMap(override val cfg: CFG, override val name: String, required: Required, criteria: Map[String, TestGenerator]) extends TestGenerator(cfg, name) {

  override def generateTests(): GeneratedTest = required match {
    case All =>
      GeneratedTestMap(criteria.map {
        case (key, crit) => key -> crit.generateTests()
      }, All, this)
    case Any => criteria.find {
      case (_, crit) => crit.generateTests().valid
    } match {
      case Some((key, c)) => GeneratedTestMap(Map(key -> c.generateTests()), Any, this)
      case None => GeneratedTestMap(Map(), Any, this)
    }
  }

  override def toString: String = super.toString + criteria.map {
    case (key, value) => key + "->" + value.toString
  }.mkString("\n")
}

case class PathTestGenerator(override val cfg: CFG, override val name: String, path: TestGenerator.Path) extends TestGenerator(cfg, name) {
  def generateTests(): GeneratedTest = {
    val constraints = Constraint.BuildConstraints(cfg, path)
    val model: Model = new Model()
    val variables = (Map[String, IntVar]() /: constraints) {
      case (accMap, exp) =>
        (accMap /: Constraint.identifyVariables(exp)) {
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
      GeneratedTestState(State(variables
        .filterKeys(!_.contains('_'))
        .mapValues(_.getValue)
      ), this)
    } else {
      GeneratedTestError("unable to solve " + constraints.mkString(" & ") + " for path " + Path(path).toString, this)
    }
  }

}
