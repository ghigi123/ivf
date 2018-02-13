package IVF.Model.AST

import IVF.Model.{AST, State}

/**
  * Contains binary expression ASTs and Operators
  * @see [[IVF.Model.AST.B.Expression]]
  */
package object B {

  import org.chocosolver.solver.Model
  import org.chocosolver.solver.variables.{BoolVar, IntVar}

  /**
    * Base class for binary expression
    * @see [[IVF.Model.AST.B.Expression.Value]]
    * @see [[IVF.Model.AST.B.Expression.Unary]]
    * @see [[IVF.Model.AST.B.Expression.Binary]]
    * @see [[IVF.Model.AST.B.Expression.Comparator]]
    */
  abstract class Expression extends AST {
    /**
      * Evaluates an binary expression given a state
      *
      * @param state state
      * @return
      */
    def eval(state: State): Boolean = this match {
      case B.Expression.Value(v) => v
      case B.Expression.Binary(op, a, b) => op match {
        case B.Operator.Or => a.eval(state) || b.eval(state)
        case B.Operator.And => a.eval(state) && b.eval(state)
      }
      case B.Expression.Unary(op, e) => op match {
        case B.Operator.Not => !e.eval(state)
      }
      case B.Expression.Comparator(op, a, b) => op match {
        case A.Comparator.GreaterEqual => a.eval(state) >= b.eval(state)
        case A.Comparator.Greater => a.eval(state) > b.eval(state)
        case A.Comparator.LessEqual => a.eval(state) <= b.eval(state)
        case A.Comparator.Less => a.eval(state) < b.eval(state)
        case A.Comparator.Equal => a.eval(state) == b.eval(state)
      }
    }

    /** Replaces a variable to another in this tree
      *
      * @param searchVar the variable to search for
      * @param newVar    the replacement variable
      * @return a new Expression with applied changes
      */
    def replace(searchVar: A.Expression.Variable, newVar: A.Expression.Variable): Expression = this match {
      case B.Expression.Comparator(comp, a, b) =>
        B.Expression.Comparator(comp, a.replace(searchVar, newVar), b.replace(searchVar, newVar))
      case B.Expression.Binary(op, a, b) =>
        B.Expression.Binary(op, a.replace(searchVar, newVar), b.replace(searchVar, newVar))
      case B.Expression.Unary(op, e) =>
        B.Expression.Unary(op, e.replace(searchVar, newVar))
      case value@B.Expression.Value(_) => value
    }

    /**
      * Transforms this AST in a variable of the solver choco representing this AST
      *
      * @param model     the choco model
      * @param variables a map from variable name to the associated choco integer variable
      * @return
      */
    def toConstraintVar(model: Model, variables: Map[String, IntVar]): BoolVar = this match {
      case B.Expression.Comparator(op, a, b) =>
        val av = a.toConstraintVar(model, variables)
        val bv = b.toConstraintVar(model, variables)
        op match {
          // choco enables to do an operation between variables and get the result as a new variable
          case A.Comparator.Equal => av.eq(bv).boolVar()
          case A.Comparator.Less => av.lt(bv).boolVar()
          case A.Comparator.LessEqual => av.le(bv).boolVar()
          case A.Comparator.Greater => av.gt(bv).boolVar()
          case A.Comparator.GreaterEqual => av.ge(bv).boolVar()
        }
      case B.Expression.Binary(op, a, b) =>
        val av = a.toConstraintVar(model, variables)
        val bv = b.toConstraintVar(model, variables)
        op match {
          case B.Operator.And => av.and(bv).boolVar()
          case B.Operator.Or => av.or(bv).boolVar()
        }
      case B.Expression.Unary(op, a) =>
        val av = a.toConstraintVar(model, variables)
        op match {
          case B.Operator.Not => av.not().boolVar()
        }
      case B.Expression.Value(tValue) =>
        model.boolVar(tValue)
    }
  }
}
