package IVF.Model.AST.B

import IVF.Model.AST.{A, B}

/**
  * Contains binary expressions
  */
package object Expression {

  /**
    * Binary binary expression
    *
    * @param op binary binary operator
    * @param a  left binary expression
    * @param b  right binary expression
    */
  case class Binary(op: B.Operator.Binary,
                    a: Expression,
                    b: Expression) extends Expression

  /**
    * Binary comparison expression
    *
    * @param op comparator
    * @param a  left arithmetic expression
    * @param b  right arithmetic expression
    */
  case class Comparator(op: A.Comparator,
                        a: A.Expression,
                        b: A.Expression) extends Expression

  /**
    * Unary binary expression
    *
    * @param op unary binary operator
    * @param a  binary expression
    */
  case class Unary(op: B.Operator.Unary,
                   a: Expression) extends Expression

  /**
    * Constant value expression
    *
    * @param tValue value
    */
  case class Value(tValue: Boolean) extends Expression

}