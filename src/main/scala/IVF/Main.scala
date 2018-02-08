package IVF

import java.io.PrintWriter

import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._

import implicits._

object Main extends App {

  Sample.samples.foreach{
    case (name, sample) =>
      val cfg = sample.ast.toCFG
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
      new PrintWriter("./graphs/" + name + ".dot") {
        write(dot)
        close()
      }

      println("Sample " + name)

      val (testGenerator, requiredCoverage) = Criterion.allKPathsCriterion(cfg, 6)
      val tests = testGenerator.generateTests()

      tests.checkError match {
        case Some(err) => println(err)
        case None => println(tests.flatten)
      }

      println(requiredCoverage)

  }

}
