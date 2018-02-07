package IVF

import java.io.PrintWriter

import org.chocosolver.solver.Model
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.variables.IntVar

import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

import implicits._

object main extends App {


  object Utils {


    def execCollectLabels(cfg: CFG, state: State): Set[Int] =
      cfg.exec(0, state).map(_.value).toSet


    def requiredNodesAllAssign(cfg: CFG): Set[Int] =
      cfg.labels.filter {
        case (_, AST.Assign(_, _)) => true
        case _ => false
      }.keys.toSet

    def foundLabels(cfg: CFG, states: Iterable[State]): Set[Int] =
      states.map(execCollectLabels(cfg, _)).reduce((a, b) => a ++ b)

    def assignCriterion(cfg: CFG, states: Iterable[State]): Set[Int] = {
      val requiredLabelsSet = requiredNodesAllAssign(cfg)
      val foundLabelsSet = foundLabels(cfg, states)
      requiredLabelsSet &~ foundLabelsSet
    }

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
      cfg.labels.getOrElse(label, AST.Skip()) match {
        case AST.While(_, _) => true
        case _ => false
      }
    }

    def isAssign(cfg: CFG, label: Int): Boolean = {
      cfg.labels.getOrElse(label, AST.Skip()) match {
        case AST.Assign(_, _) => true
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
      cfg.forwardPathBuilderAux[None.type](label, Vector(label), i, loopStates, None, (_, _, _) => (None, None))

    def iLoopPaths(cfg: CFG, label: Int, i: Int): Set[Vector[Int]] = {
      val loopStates = cfg.labels.keys.filter(isWhile(cfg, _)).map(_ -> 0).toMap
      iLoopPathsAux2(cfg, label, Vector(label), i, loopStates)
    }


    def allUsagesAux(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, path: Vector[Int]): Set[Vector[Int]] =
      cfg
        .graph
        .get(label)
        .diPredecessors
        .map(node => {
          val cmd = cfg.getAST(node.value)
          if (cmd.isDef(variable)) Set(node.value +: path)
          else if (node.value == 0) Set[Vector[Int]]()
          else allUsagesAux(cfg, node.value, variable, node.value +: path)
        })
        .foldLeft[Set[Vector[Int]]](Set[Vector[Int]]())((a, b) => a ++ b)

    def allUsagesAux2(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
      def lambda(nextNodeLabel: Int, localVariable: AST.A.Expression.Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST.A.Expression.Variable) = {
        val cmd = cfg.getAST(nextNodeLabel)
        if (cmd.isDef(variable)) (Some(Set(nextNodeLabel +: path)), localVariable)
        else if (nextNodeLabel == 0) (Some(Set[Vector[Int]]()), localVariable)
        else (None, localVariable)
      }

      val loopStates = cfg.labels.keys.filter(isWhile(cfg, _)).map(_ -> 0).toMap
      cfg.backwardPathBuilderAux(label, Vector(label), maxLoopExec, loopStates, variable, lambda)
    }


    def replaceVariables(cmd: AST.Command, variables: Map[AST.A.Expression.Variable, Int]): AST.Command =
      (cmd /: variables) {
        case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
      }

    def replaceVariables(cmd: AST.A.Expression, variables: Map[AST.A.Expression.Variable, Int]): AST.A.Expression =
      (cmd /: variables) {
        case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
      }

    def replaceVariables(cmd: AST.B.Expression, variables: Map[AST.A.Expression.Variable, Int]): AST.B.Expression =
      (cmd /: variables) {
        case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
      }

    def BuildConstraintsAux(cfg: CFG, path: Vector[Int], idx: Int, variableCounters: Map[AST.A.Expression.Variable, Int]): List[AST.B.Expression] =
      if (idx == path.length)
        List(AST.B.Expression.Value(true))
      else {
        val newVariableCounters: Map[AST.A.Expression.Variable, Int] = cfg.getAST(path(idx)) match {
          case AST.Assign(tVariable, _) =>
            variableCounters + (
              if (variableCounters.contains(tVariable))
                tVariable -> (variableCounters(tVariable) + 1)
              else
                tVariable -> 0
              )
          case _ => variableCounters
        }

        (cfg.getAST(path(idx)) match {
          case AST.Assign(tVariable, tValue) =>
            AST.B.Expression.Comparator(
              AST.A.Comparator.Equal,
              replaceVariables(tVariable, newVariableCounters),
              replaceVariables(tValue, variableCounters)
            )
          case AST.If(tCondition, _, _) =>
            cfg.next(path(idx), "true") match {
              case Some(nxt) if nxt == path.applyOrElse[Int, Int](idx + 1, (_) => -1) => replaceVariables(tCondition, variableCounters)
              case _ => AST.B.Expression.Unary(AST.B.Operator.Not, replaceVariables(tCondition, variableCounters))
            }
          case AST.While(tCondition, _) =>
            cfg.next(path(idx), "true") match {
              case Some(nxt) if nxt == path.applyOrElse[Int, Int](idx + 1, (_) => -1) => replaceVariables(tCondition, variableCounters)
              case _ => AST.B.Expression.Unary(AST.B.Operator.Not, replaceVariables(tCondition, variableCounters))
            }
          case _ => AST.B.Expression.Value(true)
        }) :: BuildConstraintsAux(cfg, path, idx + 1, newVariableCounters)
      }

    def identifyVariables(exp: AST.B.Expression): Vector[AST.A.Expression.Variable] =
      exp match {
        case AST.B.Expression.Unary(_, e) => identifyVariables(e)
        case AST.B.Expression.Binary(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
        case AST.B.Expression.Value(_) => Vector()
        case AST.B.Expression.Comparator(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
      }

    def identifyVariables(exp: AST.A.Expression): Vector[AST.A.Expression.Variable] =
      exp match {
        case AST.A.Expression.Binary(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
        case AST.A.Expression.Unary(_, a) => identifyVariables(a)
        case tVar: AST.A.Expression.Variable => Vector(tVar)
        case AST.A.Expression.Value(_) => Vector()
      }

    def BuildConstraints(cfg: CFG, path: Vector[Int]): List[AST.B.Expression] = {
      BuildConstraintsAux(cfg, path, 0, Map())
    }

  }

  val tree = AST.Sequence(List(
    AST.If(
      AST.B.Expression.Comparator(AST.A.Comparator.LessEqual, AST.A.Expression.Variable("X"), AST.A.Expression.Value(0)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Unary(AST.A.Operator.Less, AST.A.Expression.Variable("X"))),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Binary(AST.A.Operator.Less, AST.A.Expression.Value(1), AST.A.Expression.Variable("X")))
    ),
    AST.If(
      AST.B.Expression.Comparator(AST.A.Comparator.Equal, AST.A.Expression.Variable("X"), AST.A.Expression.Value(1)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Value(1)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Binary(AST.A.Operator.Plus, AST.A.Expression.Value(1), AST.A.Expression.Variable("X")))
    ),
  ))

  val tree5 = AST.Sequence(List(
    AST.If(
      AST.B.Expression.Comparator(AST.A.Comparator.LessEqual, AST.A.Expression.Variable("X"), AST.A.Expression.Value(0)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Unary(AST.A.Operator.Less, AST.A.Expression.Variable("X"))),
      AST.Sequence(List(
        AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Binary(AST.A.Operator.Less, AST.A.Expression.Value(1), AST.A.Expression.Variable("X"))),
        AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Binary(AST.A.Operator.Less, AST.A.Expression.Value(2), AST.A.Expression.Variable("X")))
      ))
    ),
    AST.If(
      AST.B.Expression.Comparator(AST.A.Comparator.Equal, AST.A.Expression.Variable("X"), AST.A.Expression.Value(1)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Value(1)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Binary(AST.A.Operator.Plus, AST.A.Expression.Value(1), AST.A.Expression.Variable("X")))
    ),
  ))

  val tree2 = AST.Sequence(List(
    AST.While(AST.B.Expression.Value(true),
      AST.If(AST.B.Expression.Value(true),
        AST.Sequence(
          List(
            AST.Skip(),
            AST.Skip()
          )
        ),
        AST.Skip()
      )
    )
  ))

  val tree6 = AST.Sequence(List(
    AST.Skip(),
    AST.While(
      AST.B.Expression.Comparator(AST.A.Comparator.GreaterEqual, AST.A.Expression.Variable("X"), AST.A.Expression.Value(2)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Binary(AST.A.Operator.Less, AST.A.Expression.Variable("X"), AST.A.Expression.Value(1))),
    ),
    AST.Assign(AST.A.Expression.Variable("Y"), AST.A.Expression.Variable("X")),
    AST.If(
      AST.B.Expression.Comparator(
        AST.A.Comparator.Greater,
        AST.A.Expression.Variable("Y"),
        AST.A.Expression.Value(2)
      ),
      AST.Skip(),
      AST.Assign(AST.A.Expression.Variable("Z"), AST.A.Expression.Variable("X")),
    )
  ))

  val tree3 = AST.Sequence(List(
    AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Value(1)),
    AST.If(
      AST.B.Expression.Comparator(AST.A.Comparator.Equal, AST.A.Expression.Variable("Y"), AST.A.Expression.Value(0)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Value(2)),
      AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Value(3))
    ),
    AST.Assign(AST.A.Expression.Variable("Y"), AST.A.Expression.Variable("X"))
  ))

  val tree4 = AST.Sequence(List(
    AST.Assign(AST.A.Expression.Variable("X"), AST.A.Expression.Value(1)),
    AST.While(
      AST.B.Expression.Value(false),
      AST.Assign(AST.A.Expression.Variable("Y"), AST.A.Expression.Value(2)),
    ),
    AST.Assign(AST.A.Expression.Variable("Y"), AST.A.Expression.Variable("X")),
    AST.Assign(AST.A.Expression.Variable("Z"), AST.A.Expression.Variable("X"))
  ))

  val tree7 = AST.Sequence(List(
    AST.Assign(AST.A.Expression.Variable("Y"), AST.A.Expression.Variable("X")),
    AST.If(
      AST.B.Expression.Value(false),
      AST.Skip(),
      AST.Skip()
    ),
    AST.Assign(AST.A.Expression.Variable("Z"), AST.A.Expression.Variable("Y")),
  ))

  val graph = tree7.toCFG

  val dotRoot = DotRootGraph(
    directed = true,
    id = Some("CFG")
  )

  def edgeTransformer(innerEdge: CFG.GraphType#EdgeT): Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
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


  val criterion = CriterionUtils.allDefinitionsCriterion(graph, 1)
  val tests = criterion.generateTests()

  tests.checkError match {
    case Some(err) => println(err)
    case None => println(tests.flatten)
  }



}
