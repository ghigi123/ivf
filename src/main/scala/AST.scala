package IVF

import org.chocosolver.solver.Model
import org.chocosolver.solver.variables.{BoolVar, IntVar}

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

sealed abstract class AST {
  override def toString: String = this match {
    case AST.A.Expression.Value(tValue) => tValue.toString
    case AST.A.Expression.Variable(tName) => tName
    case AST.Assign(tVariable, tValue) => tVariable.toString + " := " + tValue.toString
    case AST.Sequence(seq) => ("" /: seq) ((s, cmd) => s + "\n" + cmd.toString)
    case AST.If(tCondition, tThen, tElse) =>
      s"if (${tCondition.toString})\nthen\n${tThen.toString}\nelse\n${tElse.toString}\nend if"
    case AST.B.Expression.Binary(op, a, b) =>
      a.toString + (op match {
        case AST.B.Operator.Or => "|"
        case AST.B.Operator.And => "&"
      }) + b.toString
    case AST.B.Expression.Unary(op, e) =>
      (op match {
        case AST.B.Operator.Not => "!"
      }) + e.toString
    case AST.While(tCondition, tExpression) =>
      s"while (${tCondition.toString})\ndo\n${tExpression.toString}\nend while"
    case AST.B.Expression.Comparator(tOperator, a, b) =>
      a.toString + (tOperator match {
        case AST.A.Comparator.GreaterEqual => ">="
        case AST.A.Comparator.Greater => ">"
        case AST.A.Comparator.LessEqual => "<="
        case AST.A.Comparator.Less => "<"
        case AST.A.Comparator.Equal => "="
      }) + b.toString
    case AST.A.Expression.Unary(op, e) => (op match {
      case AST.A.Operator.Less => "-"
    }) + e.toString
    case AST.A.Expression.Binary(op, a, b) =>
      a.toString + (op match {
        case AST.A.Operator.Less => "-"
        case AST.A.Operator.Plus => "+"
        case AST.A.Operator.Div => "/"
        case AST.A.Operator.Times => "*"
      }) + b.toString
    case AST.B.Expression.Value(v) => v.toString
    case AST.Skip() => "skip"
    case _ => ""
  }

  def isRef(variable: AST.A.Expression.Variable): Boolean = this match {
      case AST.A.Expression.Variable(tName) if tName == variable.tName => true
      case AST.A.Expression.Variable(tName) if tName != variable.tName => false
      case AST.A.Expression.Value(_) => false
      case AST.A.Expression.Unary(_, e) => e.isRef(variable)
      case AST.A.Expression.Binary(_, a, b) => a.isRef(variable) || b.isRef(variable)
      case AST.B.Expression.Unary(_, e) => e.isRef(variable)
      case AST.B.Expression.Binary(_, a, b) => a.isRef(variable) || b.isRef(variable)
      case AST.B.Expression.Comparator(_, a, b) => a.isRef(variable) || b.isRef(variable)
      case AST.B.Expression.Value(_) => false
      case AST.If(a, _, _) => a.isRef(variable)
      case AST.While(a, _) => a.isRef(variable)
      case AST.Skip() => false
      case AST.Assign(_, tValue) => tValue.isRef(variable)
    }

  def isDef(variable: AST.A.Expression.Variable): Boolean = this match {
      case AST.Assign(tVar, _) if tVar.tName == variable.tName => true
      case _ => false
    }
}

object AST {

  object B {

    sealed abstract class Expression extends AST {
      def eval(state: State): Boolean = this match {
        case B.Expression.Value(v) => v
        case B.Expression.Binary(op, a, b) => op match {
          case B.Operator.Or => a.eval(state) || b.eval(state)
          case B.Operator.And => a.eval(state) && b.eval(state)
        }
        case B.Expression.Unary(op, e) => op match {
          case B.Operator.Not => !e.eval(state)
        }
        case B.Expression.Comparator(op, a, b) => op match {
          case A.Comparator.GreaterEqual => a.eval(state) >= b.eval(state)
          case A.Comparator.Greater => a.eval(state) > b.eval(state)
          case A.Comparator.LessEqual => a.eval(state) <= b.eval(state)
          case A.Comparator.Less => a.eval(state) < b.eval(state)
          case A.Comparator.Equal => a.eval(state) == b.eval(state)
        }
      }

      def replace(searchVar: A.Expression.Variable, newVar: A.Expression.Variable):B.Expression = this match {
        case B.Expression.Comparator(comp, a, b) =>
          B.Expression.Comparator(comp, a.replace(searchVar, newVar), b.replace(searchVar, newVar))
        case B.Expression.Binary(op, a, b) =>
          B.Expression.Binary(op, a.replace(searchVar, newVar), b.replace(searchVar, newVar))
        case B.Expression.Unary(op, e) =>
          B.Expression.Unary(op, e.replace(searchVar, newVar))
        case value@B.Expression.Value(_) => value
      }

      def toConstraintVar(model: Model, variables: Map[String, IntVar]): BoolVar = this match {
        case B.Expression.Comparator(op, a, b) =>
          val av = a.toConstraintVar(model, variables)
          val bv = b.toConstraintVar(model, variables)
          op match {
            case A.Comparator.Equal => av.eq(bv).boolVar()
            case A.Comparator.Less => av.lt(bv).boolVar()
            case A.Comparator.LessEqual => av.le(bv).boolVar()
            case A.Comparator.Greater => av.gt(bv).boolVar()
            case A.Comparator.GreaterEqual => av.ge(bv).boolVar()
          }
        case B.Expression.Binary(op, a, b) =>
          val av = a.toConstraintVar(model, variables)
          val bv = b.toConstraintVar(model, variables)
          op match {
            case B.Operator.And => av.and(bv).boolVar()
            case B.Operator.Or => av.or(bv).boolVar()
          }
        case B.Expression.Unary(op, a) =>
          val av = a.toConstraintVar(model, variables)
          op match {
            case B.Operator.Not => av.not().boolVar()
          }
        case B.Expression.Value(tValue) =>
          model.boolVar(tValue)
      }
    }

    object Expression {

      case class Binary(op: B.Operator.Binary,
                        a: Expression,
                        b: Expression) extends Expression

      case class Comparator(op: A.Comparator,
                            a: A.Expression,
                            b: A.Expression) extends Expression

      case class Unary(op: B.Operator.Unary,
                       a: Expression) extends Expression

      case class Value(tValue: Boolean) extends Expression

    }

    object Operator {

      sealed trait Unary

      sealed trait Binary

      case object Or extends Binary

      case object And extends Binary

      case object Not extends Unary

    }

  }

  object A {

    sealed abstract class Expression extends AST {
      def eval(state: State): Int = this match {
        case A.Expression.Value(v) => v
        case A.Expression.Variable(name) => state.get(name)
        case A.Expression.Binary(op, a, b) => op match {
          case A.Operator.Plus => a.eval(state) + b.eval(state)
          case A.Operator.Less => a.eval(state) - b.eval(state)
          case A.Operator.Times => a.eval(state) * b.eval(state)
          case A.Operator.Div => a.eval(state) / b.eval(state)
        }
        case A.Expression.Unary(op, e) => op match {
          case A.Operator.Less => -e.eval(state)
        }
      }

      def replace(searchVar: A.Expression.Variable, newVar: A.Expression.Variable):A.Expression = this match {
        case variable@A.Expression.Variable(_) => variable.replace(searchVar, newVar)
        case value@A.Expression.Value(_) => value
        case A.Expression.Unary(op, e) =>
          A.Expression.Unary(op, e.replace(searchVar, newVar))
        case A.Expression.Binary(op, a, b) =>
          A.Expression.Binary(op, a.replace(searchVar, newVar), b.replace(searchVar, newVar))
      }

      def toConstraintVar(model: Model, variables: Map[String, IntVar]): IntVar = this match {
        case A.Expression.Variable(tName) => variables(tName)
        case A.Expression.Value(tValue) => model.intVar(tValue)
        case A.Expression.Binary(op, a, b) =>
          val av = a.toConstraintVar(model, variables)
          val bv = b.toConstraintVar(model, variables)
          op match {
            case A.Operator.Plus => av.add(bv).intVar()
            case A.Operator.Less => av.sub(bv).intVar()
            case A.Operator.Times => av.mul(bv).intVar()
            case A.Operator.Div => av.div(bv).intVar()
          }
        case A.Expression.Unary(op, a) =>
          val av = a.toConstraintVar(model, variables)
          op match {
            case A.Operator.Less => model.intVar(0).sub(av).intVar()
          }
      }
    }

    object Expression {
      case class Binary(op: Operator.Binary,
                        a: Expression,
                        b: Expression) extends Expression

      case class Unary(op: Operator.Unary,
                       a: Expression) extends Expression

      case class Variable(tName: String) extends Expression {
        override def replace(searchVar: Variable, newVar: Variable): A.Expression.Variable = this match {
          case A.Expression.Variable(searchVar.tName) => newVar
          case variable @ A.Expression.Variable(_) => variable
        }
      }

      case class Value(tValue: Int) extends Expression

    }


    object Operator {

      sealed trait Unary

      sealed trait Binary

      case object Plus extends Binary

      case object Less extends Binary with Unary

      case object Times extends Binary

      case object Div extends Binary

    }

    sealed trait Comparator

    object Comparator {

      case object GreaterEqual extends Comparator

      case object Greater extends Comparator

      case object LessEqual extends Comparator

      case object Less extends Comparator

      case object Equal extends Comparator

    }

  }

  sealed trait Command extends AST {
    def replace(searchVar: A.Expression.Variable, newVar: A.Expression.Variable):Command = this match {
      case If(cond, tThen, tElse) =>
        If(cond.replace(searchVar, newVar), tThen.replace(searchVar, newVar), tElse.replace(searchVar, newVar))
      case While(cond, expr) =>
        While(cond.replace(searchVar, newVar), expr.replace(searchVar, newVar))
      case skip@Skip() => skip
      case Sequence(seq) =>
        Sequence(seq.map(cmd => cmd.replace(searchVar, newVar)))
      case Assign(tVariable, tValue) =>
        Assign(tVariable.replace(searchVar, newVar), tValue.replace(searchVar, newVar))
    }

    def exec(state: State): State = this match {
      case Skip() => state
      case If(tCondition, tThen, tElse) =>
        if (tCondition.eval(state))
          tThen.exec(state)
        else
          tElse.exec(state)
      case Sequence(seq) =>
        (state /: seq) ((st: State, ast: Command) => ast.exec(st))
      case wh@While(tCondition, tExpression) =>
        if (tCondition.eval(state))
          wh.exec(tExpression.exec(state))
        else
          state
      case Assign(tVariable, tExpression) =>
        state.set(tVariable.tName, tExpression.eval(state))
    }

    protected def toCFGRec(i: Int): (CFG, Int, List[Int]) = this match {
      case AST.If(_, tThen, tElse) =>
        val (tThenCfg, firstElseI, thenOuts) = tThen.toCFGRec(i + 1)
        val (tElseCfg, availableI, elseOuts) = tElse.toCFGRec(firstElseI)
        (
          tThenCfg
            .extend(tElseCfg)
            .graphOp(_
              + (i ~+> (i + 1)) ("true")
              + (i ~+> firstElseI) ("false")
              -- thenOuts.map(outI => (outI ~+> firstElseI) (""))
              ++ thenOuts.map(outI => (outI ~+> availableI) (""))
            )
            .mapOp(_ + (i -> this)),
          availableI, thenOuts ++ elseOuts
        )
      case AST.Skip() => (CFG(Graph((i ~+> (i + 1)) ("")), Map(i -> this)), i + 1, List(i))
      case AST.Assign(_, _) =>
        (
          CFG(
            Graph((i ~+> (i + 1)) ("")),
            Map(i -> this)
          ),
          i + 1,
          List(i)
        )
      case AST.Sequence(seq) =>
        ((CFG.empty(), i, List(i)) /: seq) {
          case ((accCfg, accI, _), cmd) =>
            val (subCfg, subNextI, _) = cmd.toCFGRec(accI)
            (
              accCfg
                .extend(subCfg)
                .graphOp(_ + (accI ~+> (accI + 1)) (""))
                .mapOp(_ + (accI -> cmd)),
              subNextI,
              List(accI)
            )
        }
      case AST.While(tCondition, tExpression) =>
        val (tExpCfg, tExpi, outs) = tExpression.toCFGRec(i + 1)
        (
          tExpCfg
            .graphOp(_
              + (i ~+> (i + 1)) ("true")
              + (i ~+> tExpi) ("false")
              ++ outs.map(outI => (outI ~+> i) (""))
              -- outs.map(outI => (outI ~+> tExpi) (""))
            )
            .mapOp(_ + (i -> this)),
          tExpi,
          List(i)
        )
    }

    def toCFG: CFG = this.toCFGRec(0)._1

  }

  case class If(tCondition: B.Expression, tThen: Command, tElse: Command) extends Command

  case class While(tCondition: B.Expression, tExpression: Command) extends Command

  case class Sequence(tExpressions: List[Command]) extends Command

  case class Skip() extends Command

  case class Assign(tVariable: A.Expression.Variable, tValue: A.Expression) extends Command

}