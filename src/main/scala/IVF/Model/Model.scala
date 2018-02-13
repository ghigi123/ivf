package IVF

import IVF.Model.AST._

/** Contains models necessary to run tests
  *
  *  - [[IVF.Model.AST]] contains abstract syntax trees
  *  - [[IVF.Model.CFG]] contains control flow graphs
  *  - [[IVF.Model.State]] models a state (valuation for algorithm)
  */
package object Model {

  /**
    * Base class for abstract syntax trees
    *
    * @see [[IVF.Model.AST.A.Expression]]
    * @see [[IVF.Model.AST.B.Expression]]
    * @see [[IVF.Model.AST.Command]]
    */
  abstract class AST {
    /** Returns a string representation of this AST
      *
      * Implemented here for easy update (with match clause)
      *
      * @return
      */
    override def toString: String = this match {
      case A.Expression.Value(tValue) => tValue.toString
      case A.Expression.Variable(tName) => tName
      case Assign(tVariable, tValue) => tVariable.toString + " := " + tValue.toString
      case Sequence(seq) => ("" /: seq) ((s, cmd) => s + "\n" + cmd.toString)
      case If(tCondition, tThen, tElse) =>
        s"if (${tCondition.toString})\nthen\n${tThen.toString}\nelse\n${tElse.toString}\nend if"
      case B.Expression.Binary(op, a, b) =>
        a.toString + (op match {
          case B.Operator.Or => "|"
          case B.Operator.And => "&"
        }) + b.toString
      case B.Expression.Unary(op, e) =>
        (op match {
          case B.Operator.Not => "!"
        }) + e.toString
      case While(tCondition, tExpression) =>
        s"while (${tCondition.toString})\ndo\n${tExpression.toString}\nend while"
      case B.Expression.Comparator(tOperator, a, b) =>
        a.toString + (tOperator match {
          case A.Comparator.GreaterEqual => ">="
          case A.Comparator.Greater => ">"
          case A.Comparator.LessEqual => "<="
          case A.Comparator.Less => "<"
          case A.Comparator.Equal => "="
        }) + b.toString
      case A.Expression.Unary(op, e) => (op match {
        case A.Operator.Sub => "-"
        case A.Operator.Plus => "+"
      }) + e.toString
      case A.Expression.Binary(op, a, b) =>
        a.toString + (op match {
          case A.Operator.Sub => "-"
          case A.Operator.Plus => "+"
          case A.Operator.Div => "/"
          case A.Operator.Mul => "*"
        }) + b.toString
      case B.Expression.Value(v) => v.toString
      case Skip() => "skip"
      case _ => ""
    }


    /** Checks if an AST contains a reference to a given variable
      *
      * @param variable the variable to check
      * @return
      */
    def isRef(variable: A.Expression.Variable): Boolean = this match {
      case A.Expression.Variable(tName) if tName == variable.tName => true
      case A.Expression.Variable(tName) if tName != variable.tName => false
      case A.Expression.Value(_) => false
      case A.Expression.Unary(_, e) => e.isRef(variable)
      case A.Expression.Binary(_, a, b) => a.isRef(variable) || b.isRef(variable)
      case B.Expression.Unary(_, e) => e.isRef(variable)
      case B.Expression.Binary(_, a, b) => a.isRef(variable) || b.isRef(variable)
      case B.Expression.Comparator(_, a, b) => a.isRef(variable) || b.isRef(variable)
      case B.Expression.Value(_) => false
      case If(a, _, _) => a.isRef(variable)
      case While(a, _) => a.isRef(variable)
      case Skip() => false
      case Assign(_, tValue) => tValue.isRef(variable)
    }

    /** Checks if an AST directly defines a given variable
      * @param variable the variable to check
      * @return
      */
    def isDef(variable: A.Expression.Variable): Boolean = this match {
      case Assign(tVar, _) if tVar.tName == variable.tName => true
      case _ => false
    }
  }
}