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
        "course",
        Sequence(List(
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
        )),
        List(
          SampleTest(
            AllAssignCriterion(),
            List(
              List[State](State(Map("X" -> 1)), State(Map("X" -> -1)), State(Map("X" -> -2))),
              List[State](State(Map("X" -> -1)), State(Map("X" -> -2)))
            ),
            drawGraphs = true
          ),
          SampleTest(
            AllDecisionsCriterion(),
            List(
              List[State](State(Map("X" -> 1)), State(Map("X" -> -1)), State(Map("X" -> -2))),
              List[State](State(Map("X" -> -1)), State(Map("X" -> -2)))
            )
          ),
          SampleTest(
            AllKPathCriterion(4),
            List(
              List[State](State(Map("X" -> 1)), State(Map("X" -> -1)), State(Map("X" -> -2))),
            )
          ),
          SampleTest(
            AllDefinitionsCriterion(),
            List(
              List[State](State(Map("X" -> 1)), State(Map("X" -> -1)), State(Map("X" -> -2))),
            )
          ),
          SampleTest(
            AllUsagesCriterion(),
            List(
              List[State](State(Map("X" -> 1))),
              List[State](State(Map("X" -> -2))),
            )
          ),
          SampleTest(
            AllDUPathsCriterion(),
            List(
              List[State](State(Map("X" -> 1))),
              List[State](State(Map("X" -> -2))),
              List[State](State(Map("X" -> 1)),State(Map("X" -> -2))),
            ),
            drawGraphs = true
          )
        )
      ),
      SampleAST(
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
        )),
        List(
          SampleTest(
            AllKPathCriterion(8),
            List()
          )
        )
      ),
      SampleAST(
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
}
