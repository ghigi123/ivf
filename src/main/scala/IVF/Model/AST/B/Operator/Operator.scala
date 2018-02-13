package IVF.Model.AST.B

package object Operator {

  sealed trait Unary

  sealed trait Binary

  case object Or extends Binary

  case object And extends Binary

  case object Not extends Unary

}