package IVF

import AST._
import AST.A.Operator._
import AST.A.Comparator._
import AST.A.Expression.{Value => AValue, Unary => AUnary, Binary => ABinary, Variable}
import AST.B.Expression.{Value => BValue, Unary => BUnary, Binary => BBinary, Comparator}

case class SampleAST(name: String, ast: AST.Command, testList: List[SampleTest] = List())

case class SampleTest(criterion: Criterion, statesList: List[List[State]] = List(), drawGraphs: Boolean = true)


object Sample {
  val astSamples: Map[String, SampleAST] =
    List[SampleAST](
      SampleAST(
        "max_x_y",
        If(
          Comparator(GreaterEqual, Variable("X"), Variable("Y")),
          Assign(Variable("Z"), Variable("X")),
          Assign(Variable("Z"), Variable("Y"))
        ),
        List(
          SampleTest(
            AllAssignCriterion(),
            List(
              List(State(Map("X" -> 2, "Y" -> 5)), State(Map("X" -> 5, "Y" -> 2)))
            )
          ),
          SampleTest(
            AllDefinitionsCriterion(),
            List(
              List(State(Map("X" -> 2, "Y" -> 5)), State(Map("X" -> 5, "Y" -> 2)))
            )
          ),
          SampleTest(
            AllDecisionsCriterion(),
            List(
              List(State(Map("X" -> 2, "Y" -> 5)), State(Map("X" -> 5, "Y" -> 2)))
            )
          )
        )
      ),
      SampleAST(
        "base_converter",
        If(
          Comparator(Less, Variable("X"), AValue(0)),
          Skip(),
          If(
            Comparator(LessEqual, Variable("base"), AValue(0)),
            Skip(),
            Sequence(List(
              Assign(Variable("result"), AValue(0)),
              Assign(Variable("factor"), AValue(1)),
              While(
                Comparator(GreaterEqual, Variable("X"), Variable("base")),
                Sequence(List(
                  Assign(Variable("rest"),
                    ABinary(Sub,
                      Variable("X"),
                      ABinary(Mul,
                        ABinary(Div,
                          Variable("X"),
                          Variable("base")
                        ),
                        Variable("base")
                      )
                    )
                  ),
                  Assign(Variable("result"),
                    ABinary(Plus,
                      Variable("result"),
                      ABinary(Mul,
                        Variable("rest"),
                        Variable("factor")
                      )
                    )
                  ),
                  Assign(Variable("X"),
                    ABinary(Div, Variable("X"), Variable("base"))
                  ),
                  Assign(Variable("factor"),
                    ABinary(Mul, Variable("factor"), AValue(10))
                  )
                ))
              )
            )
            )
          )
        ),
        List(
          SampleTest(
            AllAssignCriterion(),
            List(
              List(State(Map("X" -> -1, "base" -> -1)), State(Map("X" -> 2, "base" -> -1))),
              List(State(Map("X" -> 0, "base" -> 2))),
              List(State(Map("X" -> 4, "base" -> 2)))
            )
          ),
          SampleTest(
            AllDefinitionsCriterion(),
            List(
              List(State(Map("X" -> -1, "base" -> -1)), State(Map("X" -> 2, "base" -> -1))),
              List(State(Map("X" -> 0, "base" -> 2))),
              List(State(Map("X" -> 4, "base" -> 2)))
            )
          )
        )
      )
    )
      .map(s => s.name -> s).toMap
}
