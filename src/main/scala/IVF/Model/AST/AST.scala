package IVF.Model

/**
  * Contains abstract syntax tree definitions
  *
  * @see [[IVF.Model.AST]]
  */
package object AST {

  import scalax.collection.Graph
  import scalax.collection.edge.Implicits._

  /**
    * Base class for command ASTs
    *
    * @see [[IVF.Model.AST.If]]
    * @see [[IVF.Model.AST.While]]
    * @see [[IVF.Model.AST.Skip]]
    * @see [[IVF.Model.AST.Sequence]]
    * @see [[IVF.Model.AST.Assign]]
    */
  sealed trait Command extends AST {
    /** Replaces a variable to another in this tree
      *
      * @param searchVar the variable to search for
      * @param newVar    the replacement variable
      * @return a new Expression with applied changes
      */
    def replace(searchVar: A.Expression.Variable, newVar: A.Expression.Variable): Command = this match {
      case If(cond, tThen, tElse) =>
        If(cond.replace(searchVar, newVar), tThen.replace(searchVar, newVar), tElse.replace(searchVar, newVar))
      case While(cond, expr) =>
        While(cond.replace(searchVar, newVar), expr.replace(searchVar, newVar))
      case skip@Skip() => skip
      case Sequence(seq) =>
        Sequence(seq.map(cmd => cmd.replace(searchVar, newVar)))
      case Assign(tVariable, tValue) =>
        Assign(tVariable.replace(searchVar, newVar), tValue.replace(searchVar, newVar))
    }

    /**
      * Executes a command given a state
      *
      * @param state state
      * @return
      */
    def exec(state: State): State = this match {
      case Skip() => state
      case If(tCondition, tThen, tElse) =>
        if (tCondition.eval(state))
          tThen.exec(state)
        else
          tElse.exec(state)
      case Sequence(seq) =>
        (state /: seq) ((st: State, ast: Command) => ast.exec(st))
      case wh@While(tCondition, tExpression) =>
        if (tCondition.eval(state))
          wh.exec(tExpression.exec(state))
        else
          state
      case Assign(tVariable, tExpression) =>
        state.set(tVariable.tName, tExpression.eval(state))
    }

    /**
      * Transforms this AST to a CFG recursively
      *
      * Note that this function will ALWAYS return a graph with one more node at the end
      *
      * @param i the current available label
      * @return the built CFG, the next available label, and a list of outgoing connexions
      */
    def toCFGRec(i: Int): (CFG, Int, List[Int]) = this match {
      // I will extensively cover the If case, but not the following cases as they are similar but simpler
      case If(_, tThen, tElse) =>
        // here we first compute the CFG for then subtree (as the if will occupy the node i, the first available i for this AST is i+1)
        val (tThenCfg, firstElseI, thenOuts) = tThen.toCFGRec(i + 1)
        // then wi compute the CFG for the else subtree (and the previous command gave us a free label integer)
        val (tElseCfg, availableI, elseOuts) = tElse.toCFGRec(firstElseI)
        (
          tThenCfg
            .extend(tElseCfg)
            .graphOp(_
              // we add the link if -> then labeled as true
              + (i ~+> (i + 1)) ("true")
              // we add the link if -> else labeled as false
              + (i ~+> firstElseI) ("false")
              // we remove all outgoing links from then to else (this is by default as we always need to connect our last command to a
              // potential future node (the underscore node) : the last commands of then (can recursively be multiple yes) are by default connected to
              // the underscore node and this underscore node turned out to be the first i available to the else subtree)
              -- thenOuts.map(outI => (outI ~+> firstElseI) (""))
              // and we connect all then outs to the If output (same reason)
              ++ thenOuts.map(outI => (outI ~+> availableI) (""))
            )
            // we label the i node to be the If AST tree
            .mapOp(_ + (i -> this)),
          // finally all outgoing connections to underscore are the ones from then subtree + the ones from else subtree
          availableI, thenOuts ++ elseOuts
        )
      case Skip() => (CFG(Graph((i ~+> (i + 1)) ("")), Map(i -> this)), i + 1, List(i))
      case Assign(_, _) =>
        (
          CFG(
            Graph((i ~+> (i + 1)) ("")),
            Map(i -> this)
          ),
          i + 1,
          List(i)
        )
      case Sequence(seq) =>
        // note the /: operator, equivalent to the fold left function (accumulation over a different type than the one in the sequence)
        ((CFG.empty(), i, List(i)) /: seq) {
          case ((accCfg, accI, _), cmd) =>
            val (subCfg, subNextI, _) = cmd.toCFGRec(accI)
            (
              accCfg
                .extend(subCfg)
                .graphOp(_ + (accI ~+> (accI + 1)) (""))
                .mapOp(_ + (accI -> cmd)),
              subNextI,
              List(accI)
            )
        }
      case While(_, tExpression) =>
        val (tExpCfg, tExpI, outs) = tExpression.toCFGRec(i + 1)
        (
          tExpCfg
            .graphOp(_
              + (i ~+> (i + 1)) ("true")
              + (i ~+> tExpI) ("false")
              ++ outs.map(outI => (outI ~+> i) (""))
              -- outs.map(outI => (outI ~+> tExpI) (""))
            )
            .mapOp(_ + (i -> this)),
          tExpI,
          List(i)
        )
    }

    /**
      * Shorthand for [[toCFGRec]]
      * @return
      */
    def toCFG: CFG = this.toCFGRec(0)._1

  }

  /**
    * If condition
    * @param tCondition binary expression condition
    * @param tThen the command to apply if condition is true
    * @param tElse the command to apply if condition is false
    */
  case class If(tCondition: B.Expression, tThen: Command, tElse: Command) extends Command

  /**
    * While loop
    * @param tCondition the condition to keep running the loop
    * @param tExpression what to do in every iteration
    */
  case class While(tCondition: B.Expression, tExpression: Command) extends Command

  /**
    * A sequence of commands
    * @param tExpressions list of commands
    */
  case class Sequence(tExpressions: List[Command]) extends Command

  /**
    * Do nothing
    */
  case class Skip() extends Command

  /**
    * Assigns a right value to a variable
    * @param tVariable variable
    * @param tValue arithmetic expression
    */
  case class Assign(tVariable: A.Expression.Variable, tValue: A.Expression) extends Command

}