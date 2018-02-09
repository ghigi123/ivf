package IVF

import java.io.PrintWriter

import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._

import implicits._

object Main extends App {

  Sample.astSamples.foreach {
    case (name, astSample) =>
      val cfg = astSample.ast.toCFG

      val dotRoot = DotRootGraph(
        directed = true,
        id = Some(name),
        attrList = Seq(
          DotAttr("label", name)
        )
      )

      def edgeTransformer(innerEdge: CFG.GraphType#EdgeT): Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
        case LDiEdge(source, target, value: String) =>
          Some((dotRoot, DotEdgeStmt(
            cfg.nodeToString(source.value),
            cfg.nodeToString(target.value),
            value match {
              case "" => Seq()
              case _ => Seq(DotAttr("label", value.toString))
            }
          )))
      }

      val dot = cfg.graph.toDot(dotRoot, edgeTransformer = edgeTransformer)
      val fileName = s"./graphs/$name.dot"
      new PrintWriter(fileName) {
        write(dot)
        close()
      }

      println(s"Working on program $name :")
      println(s"Program $name graph written at $fileName")

      astSample.testList.foreach(sampleTest => {

        println(s"  Testing criterion `${sampleTest.criterion.name}`")
        val (testGenerator, coverage) = sampleTest.criterion.build(cfg)
        println(s"    Required coverage (${coverage match {
          case NodeCoverage(_) => "node"
          case PathCoverage(_) => "path"
          case SourceTargetCoverage(_) => "from to"
        }}): $coverage")
        if(sampleTest.statesList.nonEmpty) {
          println(s"    Testing on example state sets:")
          sampleTest.statesList.foreach(states => {
            println(s"       State set: {${states.map(st => "(" + st.values.map { case (n, v) => n + " -> " + v }.mkString(", ") + ")").mkString(", ")}}")
            val coverTest = coverage.coverTestString(cfg, states)
            println(s"         Coverage test: $coverTest")
          })
        }
        println(s"    Generating tests:")
        val generatedTests = testGenerator.generateTests()
        generatedTests.checkError match {
          case Some(err) => println(s"       Generation error message: $err")
          case None => Unit
        }
        val generatedStates = generatedTests.flatten
        println(s"       Generated state set: ${generatedStates.map(st => "(" + st.values.map { case (n, v) => n + " -> " + v }.mkString(", ") + ")").mkString(", ")}")
        val generatedCoverTest = coverage.coverTestString(cfg, generatedStates.toList)
        println(s"         Coverage test: $generatedCoverTest")
      })


  }


}
