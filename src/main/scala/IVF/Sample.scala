package IVF

import AST._
import AST.A.Operator._
import AST.A.Comparator._
import AST.A.Expression.{Value=>AValue, Unary=>AUnary, Binary=>ABinary, Variable}
import AST.B.Expression.{Value=>BValue, Unary=>BUnary, Binary=>BBinary, Comparator}

case class Sample(name: String, ast: AST.Command)

object Sample {
  val samples: Map[String, Sample] =
    List[Sample](
      Sample(
        "course", Sequence(List(
          If(
            Comparator(LessEqual, Variable("X"), AValue(0)),
            Assign(Variable("X"), AUnary(Sub, Variable("X"))),
            Assign(Variable("X"), ABinary(Sub, AValue(1), Variable("X")))
          ),
          If(
            Comparator(Equal, Variable("X"), AValue(1)),
            Assign(Variable("X"), AValue(1)),
            Assign(Variable("X"), ABinary(Plus, AValue(1), Variable("X")))
          ),
        ))
      ),
      Sample(
        "k_path", Sequence(List(
          Assign(Variable("X"), AValue(0)),
          While(BValue(true),
            If(BValue(true),
              Sequence(
                List(
                  Skip(),
                  Skip()
                )
              ),
              Skip()
            )
          )
        ))
      ),
      Sample(
        "usage_vs_du", Sequence(List(
          Assign(Variable("Y"), Variable("X")),
          If(
            BValue(false),
            Skip(),
            Skip()
          ),
          Assign(Variable("Z"), Variable("Y")),
        ))
      )
    )
      .map(s => s.name -> s).toMap

  val tree6 = Sequence(List(
    Skip(),
    While(
      Comparator(GreaterEqual, Variable("X"), AValue(2)),
      Assign(Variable("X"), ABinary(Sub, Variable("X"), AValue(1))),
    ),
    Assign(Variable("Y"), Variable("X")),
    If(
      Comparator(
        Greater,
        Variable("Y"),
        AValue(2)
      ),
      Skip(),
      Assign(Variable("Z"), Variable("X")),
    )
  ))

  val tree3 = Sequence(List(
    Assign(Variable("X"), AValue(1)),
    If(
      Comparator(Equal, Variable("Y"), AValue(0)),
      Assign(Variable("X"), AValue(2)),
      Assign(Variable("X"), AValue(3))
    ),
    Assign(Variable("Y"), Variable("X"))
  ))

  val tree4 = Sequence(List(
    Assign(Variable("X"), AValue(1)),
    While(
      BValue(false),
      Assign(Variable("Y"), AValue(2)),
    ),
    Assign(Variable("Y"), Variable("X")),
    Assign(Variable("Z"), Variable("X"))
  ))
}
