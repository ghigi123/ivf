import java.io.PrintWriter

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.io.dot._
import implicits._

case class State(values: Map[String, Int]) {
  def get(key: String): Int = this.values(key)

  def set(key: String, value: Int): State = State(this.values + (key -> value))
}

sealed trait B_BinaryOperator

case object Or_Operator extends B_BinaryOperator

case object And_Operator extends B_BinaryOperator


sealed trait B_UnaryOperator

case object Not_Operator extends B_UnaryOperator


sealed trait A_BinaryOperator

sealed trait A_UnaryOperator

case object Plus_Operator extends A_BinaryOperator

case object Less_Operator extends A_BinaryOperator with A_UnaryOperator

case object Times_Operator extends A_BinaryOperator

case object Div_Operator extends A_BinaryOperator


sealed trait A_Comparator

case object Greater_Equal_Comparator extends A_Comparator

case object Greater_Comparator extends A_Comparator

case object Less_Equal_Comparator extends A_Comparator

case object Less_Comparator extends A_Comparator


sealed abstract class AST {
  override def toString: String = Utils.ASTtoString(this)
}

abstract class AST_B_Expression extends AST

abstract class AST_A_Expression extends AST

abstract class AST_Command extends AST


case class AST_If(tCondition: AST_B_Expression, tThen: AST_Command, tElse: AST_Command) extends AST_Command

case class AST_While(tCondition: AST_B_Expression, tExpression: AST_Command) extends AST_Command

case class AST_Sequence(tExpressions: List[AST_Command]) extends AST_Command

case class AST_Skip() extends AST_Command

case class AST_Assign(tVariable: AST_A_Variable, tValue: AST_A_Expression) extends AST_Command


case class AST_B_BinaryExpression(tOperator: B_BinaryOperator, tExpressionA: AST_B_Expression, tExpressionB: AST_B_Expression) extends AST_B_Expression

case class AST_B_ComparatorExpression(tOperator: A_Comparator, tExpressionA: AST_A_Expression, tExpressionB: AST_A_Expression) extends AST_B_Expression

case class AST_B_UnaryExpression(tOperator: B_UnaryOperator, tExpression: AST_B_Expression) extends AST_B_Expression

case class AST_B_Value(tValue: Boolean) extends AST_B_Expression


case class AST_A_BinaryExpression(tOperator: A_BinaryOperator, tExpressionA: AST_A_Expression, tExpressionB: AST_A_Expression) extends AST_A_Expression

case class AST_A_UnaryExpression(tOperator: A_UnaryOperator, tExpression: AST_A_Expression) extends AST_A_Expression

case class AST_A_Variable(tName: String) extends AST_A_Expression

case class AST_A_Value(tValue: Int) extends AST_A_Expression


case class CFG(graph: Graph[Int, DiEdge], labels: Map[Int, AST_Command]) {
  def extend(graph2: Graph[Int, DiEdge], labels2: Map[Int, AST_Command]): CFG =
    CFG(graph ++ graph2, labels ++ labels2)

  def extend(cfg2: CFG): CFG =
    this.extend(cfg2.graph, cfg2.labels)

  def mapOp(op: Map[Int, AST_Command] => Map[Int, AST_Command]): CFG =
    CFG(graph, op(labels))

  def graphOp(op: Graph[Int, DiEdge] => Graph[Int, DiEdge]): CFG =
    CFG(op(graph), labels)

  def nodeToString(i: Int): String = labels.get(i) match {
    case Some(cmd) => i + " " + cmd.toString.split('\n').apply(0)
    case None => i + " _"
  }

  def edgeToString(i1: Int, i2: Int): Option[String] = labels.get(i1) match {
    case Some(AST_If(_, _, _)) => i2 - i1 match {
      case 1 => Some("then")
      case _ => Some("else")
    }
    case Some(AST_While(_, _)) => i2 - i1 match {
      case 1 => Some("do")
      case _ => Some("else")
    }
    case _ => None
  }
}

object CFG {
  def empty(): CFG = {
    new CFG(Graph(), Map())
  }
}

object Utils {

  def ASTtoString(exp: AST): String = exp match {
    case AST_A_Value(tValue) => tValue.toString
    case AST_A_Variable(tName) => tName
    case AST_Assign(tVariable, tValue) => ASTtoString(tVariable) + " := " + ASTtoString(tValue)
    case AST_Sequence(seq) => seq.foldLeft[String]("")((s, cmd) => s + "\n" + ASTtoString(cmd))
    case AST_If(tCondition, tThen, tElse) => s"if (${ASTtoString(tCondition)})\nthen\n${ASTtoString(tThen)}\nelse\n${ASTtoString(tElse)}\nend if"
    case AST_B_BinaryExpression(op, a, b) =>
      ASTtoString(a) + (op match {
        case Or_Operator => "|"
        case And_Operator => "&"
      }) + ASTtoString(b)
    case AST_While(tCondition, tExpression) => s"while (${ASTtoString(tCondition)})\ndo\n${ASTtoString(tExpression)}\nend while"
    case AST_B_ComparatorExpression(tOperator, a, b) =>
      ASTtoString(a) + (tOperator match {
        case Greater_Equal_Comparator => ">="
        case Greater_Comparator => ">"
        case Less_Equal_Comparator => "<="
        case Less_Comparator => "<"
      }) + ASTtoString(b)
    case AST_B_Value(v) => v.toString
    case _ => ""
  }

  def eval(exp: AST_B_Expression, state: State): Boolean = exp match {
    case AST_B_Value(v) => v
    case AST_B_BinaryExpression(op, a, b) => op match {
      case Or_Operator => eval(a, state) || eval(b, state)
      case And_Operator => eval(a, state) && eval(b, state)
    }
    case AST_B_UnaryExpression(op, e) => op match {
      case Not_Operator => !eval(e, state)
    }
    case AST_B_ComparatorExpression(op, a, b) => op match {
      case Greater_Equal_Comparator => eval(a, state) >= eval(b, state)
      case Greater_Comparator => eval(a, state) > eval(b, state)
      case Less_Equal_Comparator => eval(a, state) <= eval(b, state)
      case Less_Comparator => eval(a, state) < eval(b, state)
    }
  }

  def eval(exp: AST_A_Expression, state: State): Int = exp match {
    case AST_A_Value(v) => v
    case AST_A_Variable(name) => state.get(name)
    case AST_A_BinaryExpression(op, a, b) => op match {
      case Plus_Operator => eval(a, state) + eval(b, state)
      case Less_Operator => eval(a, state) - eval(b, state)
      case Times_Operator => eval(a, state) * eval(b, state)
      case Div_Operator => eval(a, state) / eval(b, state)
    }
    case AST_A_UnaryExpression(op, e) => op match {
      case Less_Operator => -eval(e, state)
    }
  }

  def exec(exp: AST_Command, state: State): State = exp match {
    case AST_Skip() => state
    case AST_If(tCondition, tThen, tElse) =>
      if (eval(tCondition, state))
        exec(tThen, state)
      else
        exec(tElse, state)
    case AST_Sequence(seq) =>
      seq.foldLeft[State](state)((st: State, ast: AST_Command) => exec(ast, st))
    case AST_While(tCondition, tExpression) =>
      if (eval(tCondition, state))
        exec(exp, exec(tExpression, state))
      else
        state
    case AST_Assign(tVariable, tExpression) =>
      state.set(tVariable.tName, eval(tExpression, state))
  }

  def ASTtoCFG_aux(exp: AST_Command, i: Int): (CFG, Int) = exp match {
    case AST_If(_, tThen, tElse) =>
      val (tThenCfg, firstElseI) = ASTtoCFG_aux(tThen, i + 1)
      val (tElseCfg, availableI) = ASTtoCFG_aux(tElse, firstElseI)
      (
        tThenCfg
          .extend(tElseCfg)
          .graphOp(_
            + i ~> (i + 1)
            + i ~> firstElseI
            + (firstElseI - 1) ~> availableI
            - (firstElseI - 1) ~> firstElseI)
          .mapOp(_ + (i -> exp)),
        availableI
      )
    case AST_Skip() => (CFG.empty(), i)
    case AST_Assign(_, _) =>
      (
        CFG(
          Graph(i ~> (i + 1)),
          Map(i -> exp)
        ),
        i + 1
      )
    case AST_Sequence(seq) =>
      seq.foldLeft[(CFG, Int)](CFG.empty(), i) {
        case ((accCfg, accI), cmd) =>
          val (subCfg, subNextI) = ASTtoCFG_aux(cmd, accI)
          (
            accCfg
              .extend(subCfg)
              .graphOp(_ + accI ~> (accI + 1))
              .mapOp(_ + (accI -> cmd)),
            subNextI
          )
      }
    case AST_While(tCondition, tExpression) =>
      val (tExpCfg, tExpi) = ASTtoCFG_aux(tExpression, i + 1)
      (
        tExpCfg
          .graphOp(_
            + i ~> (i + 1)
            + (tExpi - 1) ~> i
            + (i ~> tExpi)
            - (tExpi - 1) ~> tExpi
          )
          .mapOp(_ + (i -> exp)),
        tExpi
      )
  }

  def ASTtoCFG(exp: AST_Command): CFG = ASTtoCFG_aux(exp, 0)._1
}


object Main extends App {
  val tree = AST_Sequence(List(
    AST_Assign(AST_A_Variable("x"), AST_A_Value(1)),
    AST_If(AST_B_ComparatorExpression(Less_Equal_Comparator, AST_A_Variable("x"), AST_A_Value(2)),
      AST_Assign(AST_A_Variable("y"), AST_A_Value(2)),
      AST_Sequence(List(
        AST_If(AST_B_ComparatorExpression(Less_Equal_Comparator, AST_A_Variable("x"), AST_A_Value(2)),
          AST_Skip(),
          AST_Sequence(List(
            AST_Assign(AST_A_Variable("z"), AST_A_Value(3)),
            AST_Assign(AST_A_Variable("z"), AST_A_Value(4)),
            AST_While(AST_B_Value(false), AST_Sequence(List(
              AST_Assign(AST_A_Variable("z"), AST_A_Value(3)),
              AST_Assign(AST_A_Variable("a"), AST_A_Value(4)),
              AST_Assign(AST_A_Variable("b"), AST_A_Value(4))
            )))
          ))
        ),
        AST_Assign(AST_A_Variable("z"), AST_A_Value(4))
      ))
    ),
    AST_Assign(AST_A_Variable("t"), AST_A_Value(4))
  ))

  val graph = Utils.ASTtoCFG(tree)

  val dotRoot = DotRootGraph(
    directed = true,
    id = Some("CFG")
  )

  def edgeTransformer(innerEdge: Graph[Int, DiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case DiEdge(source, target) =>
      Some((dotRoot, DotEdgeStmt(
        graph.nodeToString(source.value),
        graph.nodeToString(target.value),
        graph.edgeToString(source.value, target.value) match {
          case Some(s) => Seq(DotAttr("label", s))
          case None => Seq()
        }
      )))
  }

  val dot = graph.graph.toDot(dotRoot, edgeTransformer = edgeTransformer)

  new PrintWriter("output.dot") {
    write(dot);
    close
  }

  println(dot)

}