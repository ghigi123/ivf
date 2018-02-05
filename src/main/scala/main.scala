import java.io.PrintWriter

import scalax.collection.Graph
import scalax.collection.io.dot._
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import implicits._

object main extends App {

  type CFGGraph = Graph[Int, LDiEdge]


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

  case object Equal_Comparator extends A_Comparator


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


  case class AST_B_BinaryExpression(tOperator: B_BinaryOperator,
                                    tExpressionA: AST_B_Expression,
                                    tExpressionB: AST_B_Expression) extends AST_B_Expression

  case class AST_B_ComparatorExpression(tOperator: A_Comparator,
                                        tExpressionA: AST_A_Expression,
                                        tExpressionB: AST_A_Expression) extends AST_B_Expression

  case class AST_B_UnaryExpression(tOperator: B_UnaryOperator, tExpression: AST_B_Expression) extends AST_B_Expression

  case class AST_B_Value(tValue: Boolean) extends AST_B_Expression


  case class AST_A_BinaryExpression(tOperator: A_BinaryOperator,
                                    tExpressionA: AST_A_Expression,
                                    tExpressionB: AST_A_Expression) extends AST_A_Expression

  case class AST_A_UnaryExpression(tOperator: A_UnaryOperator, tExpression: AST_A_Expression) extends AST_A_Expression

  case class AST_A_Variable(tName: String) extends AST_A_Expression

  case class AST_A_Value(tValue: Int) extends AST_A_Expression


  case class CFG(graph: CFGGraph, labels: Map[Int, AST_Command]) {

    private val cfg = this

    class CFGTraverser(label: Int, state: State) extends Traversable[CFGGraph#NodeT] {
      override def foreach[U](f: CFGGraph#NodeT => U): Unit = exec(cfg, label, state, f)

      def exec[U](cfg: CFG, label: Int, state: State, f: CFGGraph#NodeT => U): State = {
        val node = cfg.graph get label
        f(node)
        cfg.labels.getOrElse(label, AST_Skip()) match {
          case AST_If(cond, _, _) =>
            val next = cfg.next(label, Utils.eval(cond, state).toString).get
            exec[U](cfg, next, state, f)
          case AST_While(cond, _) =>
            val next = cfg.next(label, Utils.eval(cond, state).toString).get
            exec[U](cfg, next, state, f)
          case other =>
            cfg.next(label, "") match {
              case Some(next) => exec[U](cfg, next, Utils.exec(other, state), f)
              case None => state
            }
        }
      }
    }

    def getAST(label: Int): AST = labels.getOrElse(label, AST_Skip())

    def exec(label: Int, state: State) = new CFGTraverser(label, state)

    def extend(graph2: CFGGraph, labels2: Map[Int, AST_Command]): CFG =
      CFG(graph ++ graph2, labels ++ labels2)

    def extend(cfg2: CFG): CFG =
      this.extend(cfg2.graph, cfg2.labels)

    def mapOp(op: Map[Int, AST_Command] => Map[Int, AST_Command]): CFG =
      CFG(graph, op(labels))

    def graphOp(op: CFGGraph => CFGGraph): CFG =
      CFG(op(graph), labels)

    def nodeToString(i: Int): String = labels.get(i) match {
      case Some(cmd) => i + " " + cmd.toString.split('\n').apply(0)
      case None => i + " _"
    }

    def next(source: Int, path: String): Option[Int] =
      this.graph.get(source).edges.find(e => e.edge match {
        case LDiEdge(from, _, lbl) if lbl == path && from == source => true
        case _ => false
      }).map(_.to.value)

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
      case AST_Sequence(seq) => ("" /: seq) ((s, cmd) => s + "\n" + ASTtoString(cmd))
      case AST_If(tCondition, tThen, tElse) =>
        s"if (${ASTtoString(tCondition)})\nthen\n${ASTtoString(tThen)}\nelse\n${ASTtoString(tElse)}\nend if"
      case AST_B_BinaryExpression(op, a, b) =>
        ASTtoString(a) + (op match {
          case Or_Operator => "|"
          case And_Operator => "&"
        }) + ASTtoString(b)
      case AST_While(tCondition, tExpression) =>
        s"while (${ASTtoString(tCondition)})\ndo\n${ASTtoString(tExpression)}\nend while"
      case AST_B_ComparatorExpression(tOperator, a, b) =>
        ASTtoString(a) + (tOperator match {
          case Greater_Equal_Comparator => ">="
          case Greater_Comparator => ">"
          case Less_Equal_Comparator => "<="
          case Less_Comparator => "<"
          case Equal_Comparator => "="
        }) + ASTtoString(b)
      case AST_A_UnaryExpression(op, e) => (op match {
        case Less_Operator => "-"
      }) + ASTtoString(e)
      case AST_A_BinaryExpression(op, a, b) =>
        ASTtoString(a) + (op match {
          case Less_Operator => "-"
          case Plus_Operator => "+"
          case Div_Operator => "/"
          case Times_Operator => "*"
        }) + ASTtoString(b)
      case AST_B_Value(v) => v.toString
      case AST_Skip() => "skip"
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
        case Equal_Comparator => eval(a, state) == eval(b, state)
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
        (state /: seq) ((st: State, ast: AST_Command) => exec(ast, st))
      case AST_While(tCondition, tExpression) =>
        if (eval(tCondition, state))
          exec(exp, exec(tExpression, state))
        else
          state
      case AST_Assign(tVariable, tExpression) =>
        state.set(tVariable.tName, eval(tExpression, state))
    }

    def exec(cfg: CFG, label: Int, state: State): State = cfg.labels.getOrElse(label, AST_Skip()) match {
      case AST_If(cond, _, _) =>
        val next = cfg.next(label, eval(cond, state).toString).get
        exec(cfg, next, state)
      case AST_While(cond, _) =>
        val next = cfg.next(label, eval(cond, state).toString).get
        exec(cfg, next, state)
      case other =>
        cfg.next(label, "") match {
          case Some(next) => exec(cfg, next, exec(other, state))
          case None => state
        }
    }

    def execFold[T](init: T)(cfg: CFG, label: Int, state: State, lambda: (T, CFGGraph#NodeT) => T): (State, T) = {
      val (futureState, agg) = cfg.labels.getOrElse(label, AST_Skip()) match {
        case AST_If(cond, _, _) =>
          val next = cfg.next(label, eval(cond, state).toString).get
          execFold(init)(cfg, next, state, lambda)
        case AST_While(cond, _) =>
          val next = cfg.next(label, eval(cond, state).toString).get
          execFold(init)(cfg, next, state, lambda)
        case other =>
          cfg.next(label, "") match {
            case Some(next) => execFold(init)(cfg, next, exec(other, state), lambda)
            case None => (state, init)
          }
      }
      (futureState, lambda(agg, cfg.graph get label))
    }


    def execCollectNodes(cfg: CFG,
                         label: Int,
                         state: State,
                         criterion: CFGGraph#NodeT => Boolean): (State, Set[CFGGraph#NodeT]) =
      execFold[Set[CFGGraph#NodeT]](Set())(
        cfg,
        label,
        state,
        (set, node) => if (criterion(node)) set + node else set
      )

    def ASTtoCFG_aux(exp: AST_Command, i: Int): (CFG, Int, List[Int]) = exp match {
      case AST_If(_, tThen, tElse) =>
        val (tThenCfg, firstElseI, thenOuts) = ASTtoCFG_aux(tThen, i + 1)
        val (tElseCfg, availableI, elseOuts) = ASTtoCFG_aux(tElse, firstElseI)
        (
          tThenCfg
            .extend(tElseCfg)
            .graphOp(_
              + (i ~+> (i + 1)) ("true")
              + (i ~+> firstElseI) ("false")
              -- thenOuts.map(outI => (outI ~+> firstElseI) (""))
              ++ thenOuts.map(outI => (outI ~+> availableI) (""))
            )
            .mapOp(_ + (i -> exp)),
          availableI, thenOuts ++ elseOuts
        )
      case AST_Skip() => (CFG(Graph((i ~+> (i + 1)) ("")), Map(i -> exp)), i + 1, List(i))
      case AST_Assign(_, _) =>
        (
          CFG(
            Graph((i ~+> (i + 1)) ("")),
            Map(i -> exp)
          ),
          i + 1,
          List(i)
        )
      case AST_Sequence(seq) =>
        ((CFG.empty(), i, List(i)) /: seq) {
          case ((accCfg, accI, _), cmd) =>
            val (subCfg, subNextI, _) = ASTtoCFG_aux(cmd, accI)
            (
              accCfg
                .extend(subCfg)
                .graphOp(_ + (accI ~+> (accI + 1)) (""))
                .mapOp(_ + (accI -> cmd)),
              subNextI,
              List(accI)
            )
        }
      case AST_While(tCondition, tExpression) =>
        val (tExpCfg, tExpi, outs) = ASTtoCFG_aux(tExpression, i + 1)
        (
          tExpCfg
            .graphOp(_
              + (i ~+> (i + 1)) ("true")
              + (i ~+> tExpi) ("false")
              ++ outs.map(outI => (outI ~+> i) (""))
              -- outs.map(outI => (outI ~+> tExpi) (""))
            )
            .mapOp(_ + (i -> exp)),
          tExpi,
          List(i)
        )
    }

    def ASTtoCFG(exp: AST_Command): CFG = ASTtoCFG_aux(exp, 0)._1

    def replace(exp: AST_A_Variable, searchVar: AST_A_Variable, newVar: AST_A_Variable): AST_A_Variable = exp match {
      case AST_A_Variable(searchVar.tName) => newVar
      case AST_A_Variable(_) => exp
    }

    def replace(exp: AST_A_Expression, searchVar: AST_A_Variable, newVar: AST_A_Variable): AST_A_Expression = exp match {
      case variable@AST_A_Variable(_) => replace(variable, searchVar, newVar)
      case value@AST_A_Value(_) => value
      case AST_A_UnaryExpression(op, e) =>
        AST_A_UnaryExpression(op, replace(e, searchVar, newVar))
      case AST_A_BinaryExpression(op, a, b) =>
        AST_A_BinaryExpression(op, replace(a, searchVar, newVar), replace(b, searchVar, newVar))
    }

    def replace(exp: AST_B_Expression, searchVar: AST_A_Variable, newVar: AST_A_Variable): AST_B_Expression = exp match {
      case AST_B_ComparatorExpression(comp, a, b) =>
        AST_B_ComparatorExpression(comp, replace(a, searchVar, newVar), replace(b, searchVar, newVar))
      case AST_B_BinaryExpression(op, a, b) =>
        AST_B_BinaryExpression(op, replace(a, searchVar, newVar), replace(b, searchVar, newVar))
      case AST_B_UnaryExpression(op, e) =>
        AST_B_UnaryExpression(op, replace(e, searchVar, newVar))
      case AST_B_Value(_) => exp
    }

    def replace(exp: AST_Command, searchVar: AST_A_Variable, newVar: AST_A_Variable): AST_Command = exp match {
      case AST_If(cond, tThen, tElse) =>
        AST_If(replace(cond, searchVar, newVar), replace(tThen, searchVar, newVar), replace(tElse, searchVar, newVar))
      case AST_While(cond, expr) =>
        AST_While(replace(cond, searchVar, newVar), replace(expr, searchVar, newVar))
      case skip@AST_Skip() => skip
      case AST_Sequence(seq) =>
        AST_Sequence(seq.map(cmd => replace(cmd, searchVar, newVar)))
      case AST_Assign(tVariable, tValue) =>
        AST_Assign(replace(tVariable, searchVar, newVar), replace(tValue, searchVar, newVar))
    }

    def execCollectLabels(cfg: CFG, state: State): Set[Int] =
      graph.exec(0, state).map(_.value).toSet


    def requiredNodesAllAssign(cfg: CFG): Set[Int] =
      cfg.labels.filter {
        case (_, AST_Assign(_, _)) => true
        case _ => false
      }.keys.toSet

    def foundLabels(cfg: CFG, states: Iterable[State]): Set[Int] =
      states.map(execCollectLabels(cfg, _)).reduce((a, b) => a ++ b)

    def assignCriterion(cfg: CFG, states: Iterable[State]): Set[Int] = {
      val requiredLabelsSet = requiredNodesAllAssign(cfg)
      val foundLabelsSet = foundLabels(cfg, states)
      requiredLabelsSet &~ foundLabelsSet
    }

    def requiredNodesAllDecisions(cfg: CFG): Set[Int] = {
      cfg.graph.edges
        .filter { case LDiEdge(_, _, l) => l == "true" || l == "false" }
        .map { case LDiEdge(_, t: Int, _) => t.value }
        .toSet
    }

    def allDecisionsCriterion(cfg: CFG, states: Iterable[State]): Set[Int] =
      requiredNodesAllDecisions(cfg) &~ foundLabels(cfg, states)


    def buildKPathsAux(cfg: CFG, label: Int, path: Vector[Int], k: Int): Set[Vector[Int]] = {
      if (!cfg.labels.contains(label)) Set(path)
      else if (k == 0) Set()
      else cfg.graph.get(label).diSuccessors.map(node =>
        buildKPathsAux(cfg, node.value, path :+ node.value, k - 1)
      ).reduce((a, b) => a ++ b)
    }

    def buildKPaths(cfg: CFG, k: Int): Set[Vector[Int]] = {
      buildKPathsAux(cfg, 0, Vector(0), k)
    }

    def requiredNodesKPaths(cfg: CFG, k: Int): Set[Int] = {
      val paths = buildKPaths(cfg, k)
      (Set[Int]() /: paths) ((acc, vec) => acc ++ vec.toSet)
    }

    def kPathsCriterion(cfg: CFG, k: Int, states: Iterable[State]): Set[Int] = {
      requiredNodesKPaths(cfg, k) &~ foundLabels(cfg, states)
    }

    def isWhile(cfg: CFG, label: Int): Boolean = {
      cfg.labels.getOrElse(label, AST_Skip()) match {
        case AST_While(_, _) => true
        case _ => false
      }
    }

    def isAssign(cfg: CFG, label: Int): Boolean = {
      cfg.labels.getOrElse(label, AST_Skip()) match {
        case AST_Assign(_, _) => true
        case _ => false
      }
    }

    def iLoopPathsAux(cfg: CFG, label: Int, path: Vector[Int], i: Int, loopStates: Map[Int, Int]): Set[Vector[Int]] =
      if (!cfg.labels.contains(label)) Set(path)
      else if (!loopStates.values.forall(amt => amt <= i)) Set()
      else cfg.graph.get(label).diSuccessors.map(node => {
        val newLoopStates = if (isWhile(cfg, node.value) && label < node.value) {
          loopStates + (node.value -> 0)
        } else if (isWhile(cfg, label) && cfg.graph.find((label ~+> node.value) ("true")).nonEmpty) {
          loopStates + (label -> (loopStates(label) + 1))
        } else loopStates
        iLoopPathsAux(cfg, node.value, path :+ node.value, i, newLoopStates)
      }
      ).reduce((a, b) => a ++ b)

    def iLoopPathsAux2(cfg: CFG, label: Int, path: Vector[Int], i: Int, loopStates: Map[Int, Int]): Set[Vector[Int]] =
      forwardPathBuilderAux(cfg, label, Vector(label), i, loopStates, None, (_,_,_) => (None,None))

    def iLoopPaths(cfg: CFG, label: Int, i: Int): Set[Vector[Int]] = {
      val loopStates = cfg.labels.keys.filter(isWhile(cfg, _)).map(_ -> 0).toMap
      iLoopPathsAux2(cfg, label, Vector(label), i, loopStates)
    }

    def isDef(cmd: AST, variable: AST_A_Variable): Boolean =
      cmd match {
        case AST_Assign(tVar, _) if tVar.tName == variable.tName => true
        case _ => false
      }

    def isRef(cmd: AST, variable: AST_A_Variable): Boolean =
      cmd match {
        case AST_A_Variable(tName) if tName == variable.tName => true
        case AST_A_Variable(tName) if tName != variable.tName => false
        case AST_A_Value(_) => false
        case AST_A_UnaryExpression(_, e) => isRef(e, variable)
        case AST_A_BinaryExpression(_, a, b) => isRef(a, variable) || isRef(b, variable)
        case AST_B_UnaryExpression(_, e) => isRef(e, variable)
        case AST_B_BinaryExpression(_, a, b) => isRef(a, variable) || isRef(b, variable)
        case AST_B_ComparatorExpression(_, a, b) => isRef(a, variable) || isRef(b, variable)
        case AST_B_Value(_) => false
        case AST_If(a, _, _) => isRef(a, variable)
        case AST_While(a, _) => isRef(a, variable)
        case AST_Skip() => false
        case AST_Assign(_, tValue) => isRef(tValue, variable)
      }

    def forwardPathBuilderAux[P](cfg: CFG, label: Int, path: Vector[Int], i: Int, loopStates: Map[Int, Int], param: P, lambda: (Int, P, Vector[Int]) => (Option[Set[Vector[Int]]], P)): Set[Vector[Int]] =
      if (!cfg.labels.contains(label)) Set(path)
      else if (!loopStates.values.forall(amt => amt <= i)) Set()
      else cfg.graph.get(label).diSuccessors.map(node => {
        val newLoopStates = if (isWhile(cfg, node.value) && label < node.value) {
          loopStates + (node.value -> 0)
        } else if (isWhile(cfg, label) && cfg.graph.find((label ~+> node.value) ("true")).nonEmpty) {
          loopStates + (label -> (loopStates(label) + 1))
        } else loopStates
        val (res, nextParam) = lambda(node.value, param, path)
        res match {
          case Some(set) => set
          case None => forwardPathBuilderAux[P](cfg, node.value, path :+ node.value, i, newLoopStates, nextParam, lambda)
        }
      }
      ).reduce((a, b) => a ++ b)

    def backwardPathBuilderAux[P](cfg: CFG, label: Int, path: Vector[Int], i: Int, loopStates: Map[Int, Int], param: P, lambda: (Int, P, Vector[Int]) => (Option[Set[Vector[Int]]], P)): Set[Vector[Int]] =
      if (!cfg.labels.contains(label)) Set(path)
      else if (!loopStates.values.forall(amt => amt <= i + 1)) Set()
      else cfg.graph.get(label).diPredecessors.map(node => {
        val newLoopStates = if (isWhile(cfg, label) && label > node.value) {
          loopStates + (label -> 0)
        } else if (isWhile(cfg, node.value) && cfg.graph.find((node.value ~+> label) ("true")).nonEmpty) {
          loopStates + (node.value -> (loopStates(node.value) + 1))
        } else loopStates
        val (res, nextParam) = lambda(node.value, param, path)
        res match {
          case Some(set) => set
          case None => backwardPathBuilderAux[P](cfg, node.value, node.value +: path, i, newLoopStates, nextParam, lambda)
        }
      }
      ).foldLeft(Set[Vector[Int]]())((a, b) => a ++ b)

    def allDefinitionsAux(cfg: CFG, label: Int, variable: AST_A_Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
      def lambda(nextNodeLabel: Int, localVariable: AST_A_Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST_A_Variable) = {
        val cmd = cfg.getAST(nextNodeLabel)
        if (!cfg.labels.contains(nextNodeLabel)) (Some(Set[Vector[Int]]()), localVariable)
        else if (isRef(cmd, localVariable)) (Some(Set(path :+ nextNodeLabel)), localVariable)
        else if (isDef(cmd, localVariable)) (Some(Set[Vector[Int]]()), localVariable)
        else (None, variable)
      }

      val loopStates = cfg.labels.keys.filter(isWhile(cfg, _)).map(_ -> 0).toMap
      forwardPathBuilderAux(cfg, label, Vector(label), maxLoopExec, loopStates, variable, lambda)
    }

    def allDefinitionsPaths(cfg: CFG, maxLoopDepth: Int): Map[Int, Set[Vector[Int]]] = {
      cfg.labels
        .filterKeys(isAssign(cfg, _))
        .map {
          case (label: Int, AST_Assign(tVariable, _)) =>
            label -> allDefinitionsAux(cfg, label, tVariable, Vector(label), maxLoopDepth)
        }
    }

    def allUsagesAux(cfg: CFG, label: Int, variable: AST_A_Variable, path: Vector[Int]): Set[Vector[Int]] =
      cfg
        .graph
        .get(label)
        .diPredecessors
        .map(node => {
          val cmd = cfg.getAST(node.value)
          if (isDef(cmd, variable)) Set(node.value +: path)
          else if (node.value == 0) Set[Vector[Int]]()
          else allUsagesAux(cfg, node.value, variable, node.value +: path)
        })
        .foldLeft[Set[Vector[Int]]](Set[Vector[Int]]())((a, b) => a ++ b)

    def allUsagesAux2(cfg: CFG, label: Int, variable: AST_A_Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
      def lambda(nextNodeLabel: Int, localVariable: AST_A_Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST_A_Variable) = {
        val cmd = cfg.getAST(nextNodeLabel)
        if (isDef(cmd, variable)) (Some(Set(nextNodeLabel +: path)), localVariable)
        else if (nextNodeLabel == 0) (Some(Set[Vector[Int]]()), localVariable)
        else (None, localVariable)
      }

      val loopStates = cfg.labels.keys.filter(isWhile(cfg, _)).map(_ -> 0).toMap
      backwardPathBuilderAux(cfg, label, Vector(label), maxLoopExec, loopStates, variable, lambda)
    }

    def DUPaths(cfg: CFG, maxLoopDepth: Int): Map[AST_A_Variable, Map[Int, Set[Vector[Int]]]] = {
      val variables =
        cfg
          .labels
          .values
          .filter {
            case AST_Assign(_, _) => true
            case _ => false
          }
          .map {
            case AST_Assign(a, _) => a
          }
          .toSet

      val refs =
        variables.map { variable =>
          variable ->
            cfg
              .labels
              .filter {
                case (_, cmd) => isRef(cmd, variable)
              }
              .map {
                case (lbl, _) => lbl
              }
        }.toMap

      println(refs)

      val usagePaths = refs.map {
        case (tVar, potentialRefs) =>
          tVar -> potentialRefs
            .map { potentialRef =>
              potentialRef -> allUsagesAux2(cfg, potentialRef, tVar, Vector(potentialRef), maxLoopDepth)
            }
            .toMap
            .filter {
              case (_, set) => set.nonEmpty
            }
      }

      usagePaths
    }

  }


  val tree = AST_Sequence(List(
    AST_If(
      AST_B_ComparatorExpression(Less_Equal_Comparator, AST_A_Variable("X"), AST_A_Value(0)),
      AST_Assign(AST_A_Variable("X"), AST_A_UnaryExpression(Less_Operator, AST_A_Variable("X"))),
      AST_Assign(AST_A_Variable("X"), AST_A_BinaryExpression(Less_Operator, AST_A_Value(1), AST_A_Variable("X")))
    ),
    AST_If(
      AST_B_ComparatorExpression(Equal_Comparator, AST_A_Variable("X"), AST_A_Value(1)),
      AST_Assign(AST_A_Variable("X"), AST_A_Value(1)),
      AST_Assign(AST_A_Variable("X"), AST_A_BinaryExpression(Plus_Operator, AST_A_Value(1), AST_A_Variable("X")))
    ),
  ))

  val tree2 = AST_Sequence(List(
    AST_While(AST_B_Value(true),
      AST_If(AST_B_Value(true),
        AST_Sequence(
          List(
            AST_Skip(),
            AST_Skip()
          )
        ),
        AST_Skip()
      )
    )
  ))

  val tree3 = AST_Sequence(List(
    AST_Assign(AST_A_Variable("X"), AST_A_Value(1)),
    AST_If(
      AST_B_ComparatorExpression(Equal_Comparator, AST_A_Variable("Y"), AST_A_Value(0)),
      AST_Assign(AST_A_Variable("X"), AST_A_Value(2)),
      AST_Assign(AST_A_Variable("X"), AST_A_Value(3))
    ),
    AST_Assign(AST_A_Variable("Y"), AST_A_Variable("X"))
  ))

  val tree4 = AST_Sequence(List(
    AST_Assign(AST_A_Variable("X"), AST_A_Value(1)),
    AST_While(
      AST_B_Value(true),
      AST_Assign(AST_A_Variable("Y"), AST_A_Value(2)),
    ),
    AST_Assign(AST_A_Variable("Y"), AST_A_Variable("X")),
    AST_Assign(AST_A_Variable("Z"), AST_A_Variable("X"))
  ))

  val graph = Utils.ASTtoCFG(tree4)

  val dotRoot = DotRootGraph(
    directed = true,
    id = Some("CFG")
  )

  def edgeTransformer(innerEdge: CFGGraph#EdgeT): Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case LDiEdge(source, target, value: String) =>
      Some((dotRoot, DotEdgeStmt(
        graph.nodeToString(source.value),
        graph.nodeToString(target.value),
        value match {
          case "" => Seq()
          case _ => Seq(DotAttr("label", value.toString))
        }
      )))
  }

  val dot = graph.graph.toDot(dotRoot, edgeTransformer = edgeTransformer)


  new PrintWriter("output.dot") {
    write(dot)
    close()
  }

  println(dot)

  println(Utils.allDefinitionsPaths(graph, 1))
  println(Utils.allUsagesPath(graph, 1))


}

