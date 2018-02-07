package IVF

import java.io.PrintWriter

import org.chocosolver.solver.Model
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.variables.IntVar

import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._


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
      forwardPathBuilderAux[None.type](cfg, label, Vector(label), i, loopStates, None, (_, _, _) => (None, None))

    def iLoopPaths(cfg: CFG, label: Int, i: Int): Set[Vector[Int]] = {
      val loopStates = cfg.labels.keys.filter(isWhile(cfg, _)).map(_ -> 0).toMap
      iLoopPathsAux2(cfg, label, Vector(label), i, loopStates)
    }





    def forwardPathBuilderAux[P](cfg: CFG, label: Int,
                                 path: Vector[Int], i: Int,
                                 loopStates: Map[Int, Int],
                                 param: P,
                                 lambda: (Int, P, Vector[Int]) => (Option[Set[Vector[Int]]], P)): Set[Vector[Int]] =
      if (!cfg.labels.contains(label)) Set(path)
      else if (!loopStates.values.forall(amt => amt <= i)) Set()
      else cfg.graph.get(label)
        .diSuccessors
        .map(node => {
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
        )
        .reduce((a, b) => a ++ b)

    def backwardPathBuilderAux[P](cfg: CFG, label: Int,
                                  path: Vector[Int], i: Int,
                                  loopStates: Map[Int, Int], param: P,
                                  lambda: (Int, P, Vector[Int]) => (Option[Set[Vector[Int]]], P)): Set[Vector[Int]] =
      if (!cfg.labels.contains(label)) Set(path)
      else if (!loopStates.values.forall(amt => amt <= i + 1)) Set()
      else cfg.graph.get(label)
        .diPredecessors
        .map(node => {
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
        })
        .foldLeft(Set[Vector[Int]]())((a, b) => a ++ b)

    def allDefinitionsAux(cfg: CFG, label: Int, variable: AST.A.Expression.Variable, path: Vector[Int], maxLoopExec: Int): Set[Vector[Int]] = {
      def lambda(nextNodeLabel: Int, localVariable: AST.A.Expression.Variable, path: Vector[Int]): (Option[Set[Vector[Int]]], AST.A.Expression.Variable) = {
        val cmd = cfg.getAST(nextNodeLabel)
        if (!cfg.labels.contains(nextNodeLabel)) (Some(Set[Vector[Int]]()), localVariable)
        else if (cmd.isRef(localVariable)) (Some(Set(path :+ nextNodeLabel)), localVariable)
        else if (cmd.isDef(localVariable)) (Some(Set[Vector[Int]]()), localVariable)
        else (None, variable)
      }

      val loopStates = cfg.labels.keys.filter(isWhile(cfg, _)).map(_ -> 0).toMap
      forwardPathBuilderAux(cfg, label, Vector(label), maxLoopExec, loopStates, variable, lambda)
    }

    def allDefinitionsPaths(cfg: CFG, maxLoopDepth: Int): Map[Int, Set[Vector[Int]]] = {
      cfg.labels
        .filterKeys(isAssign(cfg, _))
        .map {
          case (label: Int, AST.Assign(tVariable, _)) =>
            label -> allDefinitionsAux(cfg, label, tVariable, Vector(label), maxLoopDepth)
        }
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
      backwardPathBuilderAux(cfg, label, Vector(label), maxLoopExec, loopStates, variable, lambda)
    }

    def DUPaths(cfg: CFG, maxLoopDepth: Int): Map[AST.A.Expression.Variable, Map[Int, Set[Vector[Int]]]] = {
      val variables =
        cfg
          .labels
          .values
          .filter {
            case AST.Assign(_, _) => true
            case _ => false
          }
          .map {
            case AST.Assign(a, _) => a
          }
          .toSet

      val refs =
        variables.map { variable =>
          variable ->
            cfg
              .labels
              .filter {
                case (_, cmd) => cmd.isRef(variable)
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
              case Some(nxt) if nxt == path(idx + 1) => replaceVariables(tCondition, variableCounters)
              case _ => AST.B.Expression.Unary(AST.B.Operator.Not, replaceVariables(tCondition, variableCounters))
            }
          case AST.While(tCondition, _) =>
            cfg.next(path(idx), "true") match {
              case Some(nxt) if nxt == path(idx + 1) => replaceVariables(tCondition, variableCounters)
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

  val graph = tree4.toCFG

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


  val kPaths = Utils.buildKPaths(graph, 5)
  kPaths.foreach(path => {
    println(path)
    val constraints = Utils.BuildConstraints(graph, path)
    val model: Model = new Model("test")
    val variables = (Map[String, IntVar]() /: constraints) {
      case (accMap, exp) =>
        (accMap /: Utils.identifyVariables(exp)) {
          case (accMap2, tVar) => if (accMap.contains(tVar.tName))
            accMap2
          else
            accMap2 + (tVar.tName -> model.intVar(tVar.tName, -100, 100))
        }
    }

    constraints.foreach((exp: AST.B.Expression) => {
      val constraint = exp.toConstraintVar(model, variables).eq(model.boolVar(true))
      println(exp)
      constraint.post()
    }
    )

    val solver = model.getSolver

    solver.setSearch(Search.minDomLBSearch(variables.values.toSeq:_*))
    if(solver.solve()) {
      println("Solution found")
      variables.foreach{
        case (name, tVar) => println(name + " : " + tVar.getValue.toString)
      }
    } else {
      solver.printStatistics()
    }
  })
}