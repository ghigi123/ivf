package IVF.TestGenerator

import IVF.Model._

/**
  * Helper functions to build constraints over paths
  */
object Constraint {

  /**
    * Replaces all variables with integer suffixed variables
    *
    * @param cmd       command
    * @param variables variable to counter
    * @return
    */
  def replaceVariables(cmd: AST.Command, variables: Map[AST.A.Expression.Variable, Int]): AST.Command =
    (cmd /: variables) {
      case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
    }

  /**
    * Replaces all variables with integer suffixed variables
    *
    * @param cmd       arithmetic expression
    * @param variables variable to counter
    * @return
    */
  def replaceVariables(cmd: AST.A.Expression, variables: Map[AST.A.Expression.Variable, Int]): AST.A.Expression =
    (cmd /: variables) {
      case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
    }

  /**
    * Replaces all variables with integer suffixed variables
    *
    * @param cmd       binary expression
    * @param variables variable to counter
    * @return
    */
  def replaceVariables(cmd: AST.B.Expression, variables: Map[AST.A.Expression.Variable, Int]): AST.B.Expression =
    (cmd /: variables) {
      case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
    }

  /**
    * Recursive function to rename variables on a path execution, (Single static assignment one a path)
    *
    * @example
    * the program X := 0; X:= 1-X becomes X_0 := 0; X_1:= 1-X_0
    * @param cfg              the graph to work on
    * @param path             the path to rename recursively
    * @param idx              the current position on the path
    * @param variableCounters current state of variables (number of times encoutered)
    * @return list of binary expression (to consider as a conjunction)
    */
  def BuildConstraintsAux(cfg: CFG, path: Vector[Int], idx: Int, variableCounters: Map[AST.A.Expression.Variable, Int]): List[AST.B.Expression] =
    if (idx == path.length)
      List(AST.B.Expression.Value(true))
    else {
      // first increase the counter when we see an assignment
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
          // return a binary comparator expression if this is an assignment
          // don't forget to rename the variables with the counters
          AST.B.Expression.Comparator(
            AST.A.Comparator.Equal,
            replaceVariables(tVariable, newVariableCounters),
            replaceVariables(tValue, variableCounters)
          )
        // if we have a if or a while, just recursive call in the right direction
        case AST.If(tCondition, _, _) =>
          // here we need to add the if condition or its opposite to the conjunction depending on which path we are going through
          cfg.next(path(idx), "true") match {
            case Some(nxt) if nxt == path.applyOrElse[Int, Int](idx + 1, (_) => -1) => replaceVariables(tCondition, variableCounters)
            case _ => AST.B.Expression.Unary(AST.B.Operator.Not, replaceVariables(tCondition, variableCounters))
          }
        case AST.While(tCondition, _) =>
          // same here
          cfg.next(path(idx), "true") match {
            case Some(nxt) if nxt == path.applyOrElse[Int, Int](idx + 1, (_) => -1) => replaceVariables(tCondition, variableCounters)
            case _ => AST.B.Expression.Unary(AST.B.Operator.Not, replaceVariables(tCondition, variableCounters))
          }
        case _ => AST.B.Expression.Value(true)
      }) :: BuildConstraintsAux(cfg, path, idx + 1, newVariableCounters)
    }

  /**
    * Identifies which variables are present in a expression
    *
    * @param exp the expression to look into
    * @return
    */
  def identifyVariables(exp: AST.B.Expression): Vector[AST.A.Expression.Variable] =
    exp match {
      case AST.B.Expression.Unary(_, e) => identifyVariables(e)
      case AST.B.Expression.Binary(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
      case AST.B.Expression.Value(_) => Vector()
      case AST.B.Expression.Comparator(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
    }

  /**
    * Identifies which variables are present in a expression
    *
    * @param exp the expression to look into
    * @return
    */
  def identifyVariables(exp: AST.A.Expression): Vector[AST.A.Expression.Variable] =
    exp match {
      case AST.A.Expression.Binary(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
      case AST.A.Expression.Unary(_, a) => identifyVariables(a)
      case tVar: AST.A.Expression.Variable => Vector(tVar)
      case AST.A.Expression.Value(_) => Vector()
    }

  /**
    * Shorthand to [[BuildConstraintsAux]]
    * @param cfg the graph to work on
    * @param path the path to rename recursively
    * @return
    */
  def BuildConstraints(cfg: CFG, path: Vector[Int]): List[AST.B.Expression] = {
    BuildConstraintsAux(cfg, path, 0, Map())
  }

}