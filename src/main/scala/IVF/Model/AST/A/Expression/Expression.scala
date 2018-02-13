package IVF.Model.AST.A

import IVF.Model.AST.A

/**
  * Contains arithmetic expressions
  */
package object Expression {

  /**
    * Binary arithmetic expression
    * @param op binary arithmetic operator
    * @param a left arithmetic expression
    * @param b right arithmetic expression
    */
  case class Binary(op: Operator.Binary,
                    a: Expression,
                    b: Expression) extends Expression

  /**
    * Unary arithmetic expression
    * @param op unary arithmetic operator
    * @param a arithmetic expression
    */
  case class Unary(op: Operator.Unary,
                   a: Expression) extends Expression

  /**
    * Variable expression
    * @param tName name of variable
    */
  case class Variable(tName: String) extends Expression {
    override def replace(searchVar: Variable, newVar: Variable): A.Expression.Variable = this match {
      case A.Expression.Variable(searchVar.tName) => newVar
      case variable@A.Expression.Variable(_) => variable
    }
  }

  /**
    * Constant value expression
    * @param tValue value
    */
  case class Value(tValue: Int) extends Expression

}