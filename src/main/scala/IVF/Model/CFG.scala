package IVF.Model

import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge

/** Describes a valuation for a program execution
  *
  * @param values a map from variable name to its value
  */
case class State(values: Map[String, Int]) {

  /** Get value from variable name
    *
    * @param key variable name
    * @return variable value
    */
  def get(key: String): Int = this.values(key)

  /** Set variable value from name
    *
    * @param key   variable name
    * @param value variable value
    * @return a new state with changes applied
    */
  def set(key: String, value: Int): State = State(this.values + (key -> value))
}

/** Utils related to [[IVF.Model.CFG]] */
object CFG {
  /** Shortcut to define the scala graph type */
  type GraphType = Graph[Int, LDiEdge]

  /** Returns a new empty CFG
    *
    * @return
    */
  def empty(): CFG = {
    new CFG(Graph(), Map())
  }
}

/** Describes a control flow graph
  *
  * Contains a graph with integer labels and labeled directed edges
  *
  * Each node corresponds to an AST stored in a {{{Map[Int, AST.Command]}}}
  *
  * @param graph  the scala graph containing integers
  * @param labels map from int label to AST
  */

case class CFG(graph: CFG.GraphType, labels: Map[Int, AST.Command]) {

  private val cfg = this

  /** An easy way to traverse a cfg execution and apply Traversable methods
    *
    * @param label label to start the execution at
    * @param state state used at the beginning of the execution
    */
  class CFGTraverser(label: Int, state: State) extends Traversable[CFG.GraphType#NodeT] {
    override def foreach[U](f: CFG.GraphType#NodeT => U): Unit = exec(cfg, label, state, f)

    /** Executes a given CFG using a starting label and a starting state recursively and applies a generic function to it on every node encountered
      *
      * @param cfg   cfg to execute
      * @param label starting label
      * @param state starting state
      * @param f     generic function to apply
      * @tparam U return type of f
      * @return
      */
    def exec[U](cfg: CFG, label: Int, state: State, f: CFG.GraphType#NodeT => U): State = {
      val node = cfg.graph get label
      // here we call f on the current node
      f(node)
      cfg.labels.getOrElse(label, AST.Skip()) match {
        // always get the next node to execute and call recursively exec
        case AST.If(cond, _, _) =>
          val next = cfg.next(label, cond.eval(state).toString).get
          exec[U](cfg, next, state, f)
        case AST.While(cond, _) =>
          val next = cfg.next(label, cond.eval(state).toString).get
          exec[U](cfg, next, state, f)
        case other =>
          cfg.next(label, "") match {
            case Some(next) => exec[U](cfg, next, other.exec(state), f)
            case None => state
          }
      }
    }
  }

  /** Returns the AST located at a given label
    *
    * @param label label
    * @return
    */
  def getAST(label: Int): AST = labels.getOrElse(label, AST.Skip())

  /** Shorthand for [[IVF.Model.CFG.CFGTraverser]]
    *
    * @param label label to start the execution at
    * @param state state used at the beginning of the execution
    * @return
    */
  def exec(label: Int, state: State) = new CFGTraverser(label, state)

  /** Returns a new CFG made of this extended with a new graph and some new labels
    *
    * @param graph2  the graph to merge
    * @param labels2 the labels to merge
    * @return
    */
  def extend(graph2: CFG.GraphType, labels2: Map[Int, AST.Command]): CFG =
    CFG(graph ++ graph2, labels ++ labels2)

  /** Returns a new CFG made of this extended with a new CFG
    *
    * @param cfg2 the CFG to merge
    * @return
    */
  def extend(cfg2: CFG): CFG =
    this.extend(cfg2.graph, cfg2.labels)

  /** Returns a new CFG whose labels are changed by the lambda given
    *
    * @param op transforms the labels
    * @return
    */
  def mapOp(op: Map[Int, AST.Command] => Map[Int, AST.Command]): CFG =
    CFG(graph, op(labels))

  /** Returns a new CFG whose graph is changed by the lambda given
    *
    * @param op transforms the graph
    * @return
    */
  def graphOp(op: CFG.GraphType => CFG.GraphType): CFG =
    CFG(op(graph), labels)

  /** Get the first line of an AST for graph display
    *
    * @param i label
    * @return
    */
  def nodeToString(i: Int): String = labels.get(i) match {
    case Some(cmd) => i + " " + cmd.toString.split('\n').apply(0)
    case None => i + " _"
  }

  /** Finds the next step in graph execution
    *
    * @param source the current node
    * @param path   if several paths are available, then this is necessary to choose the path ("" or "true" or "false")
    * @return Some(Int) if a path is found None else
    */
  def next(source: Int, path: String): Option[Int] =
    this.graph.get(source).edges.find(e => e.edge match {
      case LDiEdge(from, _, lbl) if lbl == path && from == source => true
      case _ => false
    }).map(_.to.value)

  /** Provides valid starting loop states required in [[IVF.Model.CFG.forwardPathBuilderAux]] and [[IVF.Model.CFG.backwardPathBuilderAux]]
    *
    * @return initial loop states
    */
  def emptyLoopStates: Map[Int, Int] = cfg.labels.keys.filter(cfg.isWhile).map(_ -> 0).toMap

  /** Offers all possible paths from a source to a subpath
    *
    * @param path        the subpath to fulfill up
    * @param maxLoopExec the maximum execution number of each loop during computation
    * @return
    */
  def fillPathUp(path: Vector[Int], maxLoopExec: Int): Vector[Vector[Int]] = {
    cfg.backwardPathBuilderAux[None.type](path(0), Vector(), maxLoopExec, cfg.emptyLoopStates, None, (_, _, _) => (None, None))
      .map(vector => vector ++ path).toVector
  }

  /** Checks if a node is a While AST
    *
    * @param label node
    * @return
    */
  def isWhile(label: Int): Boolean = {
    cfg.labels.getOrElse(label, AST.Skip()) match {
      case AST.While(_, _) => true
      case _ => false
    }
  }

  /** Checks if a node is an Assign AST
    *
    * @param label node
    * @return
    */
  def isAssign(label: Int): Boolean = {
    cfg.labels.getOrElse(label, AST.Skip()) match {
      case AST.Assign(_, _) => true
      case _ => false
    }
  }

  /** Recursive function used to build paths from a source in the forward direction
    *
    * Automatically returns the full path when arriving to graph end
    * Automatically returns no path when the maximum number of loops is exceeded
    *
    * @example if you want to stop the graph exploration when you find a ref node for a variable A
    *          {{{
    *          def lambda(node, A, path) =
    *              if(cfg.getAST(node).isDef(A))
    *                  Some((path, A))
    *              else
    *                  None
    *
    *          forwardPathBuilderAux(0,
                               Vector(0),
                               1,
                               cfg.emptyLoopStates,
                               AST.A.Variable("X"),
                               lambda)
    *
    *          }}}
    *
    *          here the generic type can be [[IVF.Model.AST.A.Expression.Variable]]
    * @param label      the current / start node
    * @param path       the path built from source to current node
    * @param i          the maximum loop execution number
    * @param loopStates loop states (for each loop node id, the number of times we already went through the loop)
    * @param param      a generic parameter to give to lambda
    * @param lambda     the stop condition (to implement a given condition. Must return Some(path) if the path is finished, None if we let the exploration continue and the next generic parameter to use for next step
    * @tparam P lambda parameter and return type
    * @return a set of all the paths found matching stop condition
    */
  def forwardPathBuilderAux[P](label: Int,
                               path: Vector[Int], i: Int,
                               loopStates: Map[Int, Int],
                               param: P,
                               lambda: (Int, P, Vector[Int]) => (Option[Set[Vector[Int]]], P)): Set[Vector[Int]] =
    if (!cfg.labels.contains(label)) Set(path)
    else if (!loopStates.values.forall(amt => amt <= i)) Set()
    else cfg.graph.get(label)
      .diSuccessors
      .map(node => {
        val newLoopStates = if (cfg.isWhile(node.value) && label < node.value) {
          loopStates + (node.value -> 0)
        } else if (cfg.isWhile(label) && cfg.graph.find((label ~+> node.value) ("true")).nonEmpty) {
          loopStates + (label -> (loopStates(label) + 1))
        } else loopStates
        val (res, nextParam) = lambda(node.value, param, path)
        res match {
          case Some(set) => set
          case None => cfg.forwardPathBuilderAux[P](node.value, path :+ node.value, i, newLoopStates, nextParam, lambda)
        }
      }
      )
      .reduce((a, b) => a ++ b)

  /** Recursive function used to build paths from a source in the backward direction
    *
    * Automatically returns the full path when arriving to graph start (note: place a skip as first instruction if you don't want some criterion to fail)
    * Automatically returns no path when the maximum number of loops is exceeded
    *
    * @example if you want to stop the graph exploration when you find a ref node for a variable A
    *          {{{
    *          def lambda(node, A, path) =
    *              if(cfg.getAST(node).isDef(A))
    *                  Some((path, A))
    *              else
    *                  None
    *
    *          backwardPathBuilderAux(0,
                               Vector(0),
                               1,
                               cfg.emptyLoopStates,
                               AST.A.Variable("X"),
                               lambda)
    *
    *          }}}
    *
    *          here the generic type can be [[IVF.Model.AST.A.Expression.Variable]]
    * @param label      the current / start node
    * @param path       the path built from source to current node
    * @param i          the maximum loop execution number
    * @param loopStates loop states (for each loop node id, the number of times we already went through the loop)
    * @param param      a generic parameter to give to lambda
    * @param lambda     the stop condition (to implement a given condition. Must return Some(path) if the path is finished, None if we let the exploration continue and the next generic parameter to use for next step
    * @tparam P lambda parameter and return type
    * @return a set of all the paths found matching stop condition
    */

  def backwardPathBuilderAux[P](label: Int,
                                path: Vector[Int], i: Int,
                                loopStates: Map[Int, Int], param: P,
                                lambda: (Int, P, Vector[Int]) => (Option[Set[Vector[Int]]], P)): Set[Vector[Int]] =
    if (cfg.graph.get(label).diPredecessors.isEmpty) Set(path)
    else if (!loopStates.values.forall(amt => amt <= i + 1)) Set()
    else cfg.graph.get(label)
      .diPredecessors
      .map(node => {
        val newLoopStates = if (cfg.isWhile(label) && label > node.value) {
          loopStates + (label -> 0)
        } else if (cfg.isWhile(node.value) && cfg.graph.find((node.value ~+> label) ("true")).nonEmpty) {
          loopStates + (node.value -> (loopStates(node.value) + 1))
        } else loopStates
        val (res, nextParam) = lambda(node.value, param, path)
        res match {
          case Some(set) => set
          case None => cfg.backwardPathBuilderAux[P](node.value, node.value +: path, i, newLoopStates, nextParam, lambda)
        }
      })
      .foldLeft(Set[Vector[Int]]())((a, b) => a ++ b)
}
