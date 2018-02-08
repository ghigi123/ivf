package IVF

import java.io.PrintWriter

import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._

import implicits._

object main extends App {


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

  val graph = tree.toCFG

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


  val criterion = CriterionUtils.allKPathsCriterion(graph, 6)
  val tests = criterion.generateTests()

  tests.checkError match {
    case Some(err) => println(err)
    case None => println(tests.flatten)
  }



}
