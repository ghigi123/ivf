package IVF.Model.AST.A

package object Operator {

  sealed trait Unary

  sealed trait Binary

  case object Plus extends Binary with Unary

  case object Sub extends Binary with Unary

  case object Mul extends Binary

  case object Div extends Binary

}