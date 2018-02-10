package IVF

import java.io.PrintWriter

import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._
import implicits._

object Main extends App {

  def exportGraph(cfg: CFG, name: String, label: String = "", redCoverageUnits: Seq[CoverageUnit] = Seq(), greenCoverageUnits: Seq[CoverageUnit] = Seq()): String = {
    val redNodes = redCoverageUnits.filter {
      case Node(_) => true
      case _ => false
    }.map {
      case Node(a) => Node(a)
    }

    val greenNodes = greenCoverageUnits.filter {
      case Node(_) => true
      case _ => false
    }.map {
      case Node(a) => Node(a)
    }

    val redEdges = redCoverageUnits.filter {
      case Path(_) => true
      case _ => false
    }.flatMap {
      case Path(a) => a.sliding(2).toVector
    }

    val greenEdges = greenCoverageUnits.filter {
      case Path(_) => true
      case _ => false
    }.flatMap {
      case Path(a) => a.sliding(2).toVector
    }

    val redSourceTarget = redCoverageUnits.filter {
      case SourceTarget(_, _) => true
      case _ => false
    }.map {
      case a@SourceTarget(_, _) => a
    }

    val greenSourceTarget = greenCoverageUnits.filter {
      case SourceTarget(_, _) => true
      case _ => false
    }.map {
      case a@SourceTarget(_, _) => a
    }

    val newEdges = greenSourceTarget
      .filter {
        case SourceTarget(Some(s), Some(t)) =>true
        case _ => false
      }
      .map {
        case SourceTarget(Some(source), Some(target)) => LDiEdge(source, target)("def")
      }.toList

    val cfgWithAux = (cfg/:newEdges){
      case (cfgAcc, edge) => cfgAcc.graphOp(_ + edge)
    }

    val dotRoot = DotRootGraph(
      directed = true,
      id = Some(name),
      attrList = Seq(
        DotAttr("label", label + "\n" + name)
      )
    )

    def edgeTransformer(innerEdge: CFG.GraphType#EdgeT): Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
      case LDiEdge(source, target, value: String) =>
        Some((dotRoot, DotEdgeStmt(
          cfgWithAux.nodeToString(source.value),
          cfgWithAux.nodeToString(target.value),
          (value match {
            case "" => Seq()
            case _ => Seq(DotAttr("label", value.toString))
          }) ++ (
            if (redEdges.contains(Vector(source.value, target.value)))
              Seq(DotAttr("color", "red"))
            else if (greenEdges.contains(Vector(source.value, target.value)))
              Seq(DotAttr("color", "green"))
            else
              Seq()
            )
        )))
    }

    def nodeTransformer(innerNode: CFG.GraphType#NodeT): Option[(DotGraph, DotNodeStmt)] =
      Some(dotRoot, DotNodeStmt(
        cfgWithAux.nodeToString(innerNode.value),
        (if (redNodes.exists(_.node == innerNode.value))
          Seq(DotAttr("color", "red"))
        else if (greenNodes.exists(_.node == innerNode.value))
          Seq(DotAttr("color", "green"))
        else
          Seq())
          ++ Seq(DotAttr("style", "bold"))
      ))


    val dot = cfgWithAux.graph.toDot(dotRoot, edgeTransformer = edgeTransformer, cNodeTransformer = Some(nodeTransformer))
    val fileName = s"./graphs/$name.dot"
    new PrintWriter(fileName) {
      write(dot)
      close()
    }
    fileName
  }

  Sample.astSamples.foreach {
    case (name, astSample) =>
      val cfg = astSample.ast.toCFG


      println(s"Working on program $name :")
      println(s"Program $name graph written at ${exportGraph(cfg, name)}")

      astSample.testList.foreach(sampleTest => {

        println(s"  Testing criterion `${sampleTest.criterion.name}`")
        val (testGenerator, coverage) = sampleTest.criterion.build(cfg)
        println(s"    Required coverage (${
          coverage match {
            case NodeCoverage(_) => "node"
            case PathCoverage(_) => "path"
            case SourceTargetCoverage(_) => "from to"
            case SourceAnyTargetCoverage(_) => "from to any of"
          }
        }): $coverage")
        if (sampleTest.statesList.nonEmpty) {
          println(s"    Testing on example state sets:")
          sampleTest.statesList.zipWithIndex.foreach {
            case (states, idx) =>
              val stateString = states.map(st => "(" + st.values.map { case (n, v) => n + " -> " + v }.mkString(", ") + ")").mkString(", ")
              println(s"       State set: {$stateString")
              val coverTest = coverage.coverTestString(cfg, states)
              println(s"         Coverage test: $coverTest")
              if (sampleTest.drawGraphs)
                println("         Exported graph for test at " + exportGraph(cfg, name + "_" + sampleTest.criterion.name.replace(' ', '_') + "_test_" + idx,
                  redCoverageUnits = coverage.coverTest(cfg, states).toSeq,
                  greenCoverageUnits = coverage match {
                    case NodeCoverage(set) => set.toSeq
                    case PathCoverage(set) => set.toSeq
                    case SourceTargetCoverage(set) => set.toSeq
                    case SourceAnyTargetCoverage(set) => set.toSeq
                  },
                  label = stateString
                ))
          }
        }
        println(s"    Generating tests:")
        val generatedTests = testGenerator.generateTests()
        generatedTests.checkError match {
          case Some(err) => println(s"       Generation error message: $err")
          case None => Unit
        }
        val generatedStates = generatedTests.flatten
        println(s"       Generated state set: {${generatedStates.map(st => "(" + st.values.map { case (n, v) => n + " -> " + v }.mkString(", ") + ")").mkString(", ")}}")
        val generatedCoverTest = coverage.coverTestString(cfg, generatedStates.toList)
        println(s"         Coverage test: $generatedCoverTest")
      })


  }


}
