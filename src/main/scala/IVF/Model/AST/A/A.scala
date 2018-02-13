package IVF.Model.AST

import IVF.Model.{AST, State}
import org.chocosolver.solver.Model
import org.chocosolver.solver.variables.IntVar

/**
  * Contains arithmetic expression ASTs and Operators
  * @see [[IVF.Model.AST.A.Expression]]
  */
package object A {

  /**
    * Base class for arithmetic expression
    * @see [[IVF.Model.AST.A.Expression.Variable]]
    * @see [[IVF.Model.AST.A.Expression.Value]]
    * @see [[IVF.Model.AST.A.Expression.Binary]]
    * @see [[IVF.Model.AST.A.Expression.Unary]]
    */
  abstract class Expression extends AST {
    /**
      * Evaluates an arithmetic expression given a state
      *
      * @param state state
      * @return
      */
    def eval(state: State): Int = this match {
      case A.Expression.Value(v) => v
      case A.Expression.Variable(name) => state.get(name)
      case A.Expression.Binary(op, a, b) => op match {
        case A.Operator.Plus => a.eval(state) + b.eval(state)
        case A.Operator.Sub => a.eval(state) - b.eval(state)
        case A.Operator.Mul => a.eval(state) * b.eval(state)
        case A.Operator.Div => a.eval(state) / b.eval(state)
      }
      case A.Expression.Unary(op, e) => op match {
        case A.Operator.Sub => -e.eval(state)
        case A.Operator.Plus => e.eval(state)
      }
    }

    /** Replaces a variable to another in this tree
      *
      * @param searchVar the variable to search for
      * @param newVar    the replacement variable
      * @return a new Expression with applied changes
      */
    def replace(searchVar: A.Expression.Variable, newVar: A.Expression.Variable): Expression = this match {
      case variable@A.Expression.Variable(_) => variable.replace(searchVar, newVar)
      case value@A.Expression.Value(_) => value
      case A.Expression.Unary(op, e) =>
        A.Expression.Unary(op, e.replace(searchVar, newVar))
      case A.Expression.Binary(op, a, b) =>
        A.Expression.Binary(op, a.replace(searchVar, newVar), b.replace(searchVar, newVar))
    }

    /**
      * Transforms this AST in a variable of the solver choco representing this AST
      *
      * @param model     the choco model
      * @param variables a map from variable name to the associated choco integer variable
      * @return
      */
    def toConstraintVar(model: Model, variables: Map[String, IntVar]): IntVar = this match {
      case A.Expression.Variable(tName) => variables(tName)
      case A.Expression.Value(tValue) => model.intVar(tValue)
      case A.Expression.Binary(op, a, b) =>
        val av = a.toConstraintVar(model, variables)
        val bv = b.toConstraintVar(model, variables)
        op match {
          // choco enables to do an operation between variables and get the result as a new variable
          case A.Operator.Plus => av.add(bv).intVar()
          case A.Operator.Sub => av.sub(bv).intVar()
          case A.Operator.Mul => av.mul(bv).intVar()
          case A.Operator.Div => av.div(bv).intVar()
        }
      case A.Expression.Unary(op, a) =>
        val av = a.toConstraintVar(model, variables)
        op match {
          case A.Operator.Sub => model.intVar(0).sub(av).intVar()
          case A.Operator.Plus => av
        }
    }
  }

  /**
    * A base trait for comparator case objects
    */
  trait Comparator

}