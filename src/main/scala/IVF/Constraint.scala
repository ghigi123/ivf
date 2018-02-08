package IVF

object Constraint {

  def replaceVariables(cmd: AST.Command, variables: Map[AST.A.Expression.Variable, Int]): AST.Command =
    (cmd /: variables) {
      case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
    }

  def replaceVariables(cmd: AST.A.Expression, variables: Map[AST.A.Expression.Variable, Int]): AST.A.Expression =
    (cmd /: variables) {
      case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
    }

  def replaceVariables(cmd: AST.B.Expression, variables: Map[AST.A.Expression.Variable, Int]): AST.B.Expression =
    (cmd /: variables) {
      case (accCmd, (tVar, tIdx)) => accCmd.replace(tVar, AST.A.Expression.Variable(tVar + "_" + tIdx.toString))
    }

  def BuildConstraintsAux(cfg: CFG, path: Vector[Int], idx: Int, variableCounters: Map[AST.A.Expression.Variable, Int]): List[AST.B.Expression] =
    if (idx == path.length)
      List(AST.B.Expression.Value(true))
    else {
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
          AST.B.Expression.Comparator(
            AST.A.Comparator.Equal,
            replaceVariables(tVariable, newVariableCounters),
            replaceVariables(tValue, variableCounters)
          )
        case AST.If(tCondition, _, _) =>
          cfg.next(path(idx), "true") match {
            case Some(nxt) if nxt == path.applyOrElse[Int, Int](idx + 1, (_) => -1) => replaceVariables(tCondition, variableCounters)
            case _ => AST.B.Expression.Unary(AST.B.Operator.Not, replaceVariables(tCondition, variableCounters))
          }
        case AST.While(tCondition, _) =>
          cfg.next(path(idx), "true") match {
            case Some(nxt) if nxt == path.applyOrElse[Int, Int](idx + 1, (_) => -1) => replaceVariables(tCondition, variableCounters)
            case _ => AST.B.Expression.Unary(AST.B.Operator.Not, replaceVariables(tCondition, variableCounters))
          }
        case _ => AST.B.Expression.Value(true)
      }) :: BuildConstraintsAux(cfg, path, idx + 1, newVariableCounters)
    }

  def identifyVariables(exp: AST.B.Expression): Vector[AST.A.Expression.Variable] =
    exp match {
      case AST.B.Expression.Unary(_, e) => identifyVariables(e)
      case AST.B.Expression.Binary(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
      case AST.B.Expression.Value(_) => Vector()
      case AST.B.Expression.Comparator(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
    }

  def identifyVariables(exp: AST.A.Expression): Vector[AST.A.Expression.Variable] =
    exp match {
      case AST.A.Expression.Binary(_, a, b) => identifyVariables(a) ++ identifyVariables(b)
      case AST.A.Expression.Unary(_, a) => identifyVariables(a)
      case tVar: AST.A.Expression.Variable => Vector(tVar)
      case AST.A.Expression.Value(_) => Vector()
    }

  def BuildConstraints(cfg: CFG, path: Vector[Int]): List[AST.B.Expression] = {
    BuildConstraintsAux(cfg, path, 0, Map())
  }

}