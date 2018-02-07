package IVF

import scalax.collection.Graph
import scalax.collection.edge.LDiEdge

case class State(values: Map[String, Int]) {
  def get(key: String): Int = this.values(key)

  def set(key: String, value: Int): State = State(this.values + (key -> value))
}

object CFG {
  type GraphType = Graph[Int, LDiEdge]

  def empty(): CFG = {
    new CFG(Graph(), Map())
  }
}

case class CFG(graph: CFG.GraphType, labels: Map[Int, AST.Command]) {

  private val cfg = this

  class CFGTraverser(label: Int, state: State) extends Traversable[CFG.GraphType#NodeT] {
    override def foreach[U](f: CFG.GraphType#NodeT => U): Unit = exec(cfg, label, state, f)

    def exec[U](cfg: CFG, label: Int, state: State, f: CFG.GraphType#NodeT => U): State = {
      val node = cfg.graph get label
      f(node)
      cfg.labels.getOrElse(label, AST.Skip()) match {
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

  def getAST(label: Int): AST = labels.getOrElse(label, AST.Skip())

  def exec(label: Int, state: State) = new CFGTraverser(label, state)

  def extend(graph2: CFG.GraphType, labels2: Map[Int, AST.Command]): CFG =
    CFG(graph ++ graph2, labels ++ labels2)

  def extend(cfg2: CFG): CFG =
    this.extend(cfg2.graph, cfg2.labels)

  def mapOp(op: Map[Int, AST.Command] => Map[Int, AST.Command]): CFG =
    CFG(graph, op(labels))

  def graphOp(op: CFG.GraphType => CFG.GraphType): CFG =
    CFG(op(graph), labels)

  def nodeToString(i: Int): String = labels.get(i) match {
    case Some(cmd) => i + " " + cmd.toString.split('\n').apply(0)
    case None => i + " _"
  }

  def next(source: Int, path: String): Option[Int] =
    this.graph.get(source).edges.find(e => e.edge match {
      case LDiEdge(from, _, lbl) if lbl == path && from == source => true
      case _ => false
    }).map(_.to.value)

  def edgeToString(i1: Int, i2: Int): Option[String] = labels.get(i1) match {
    case Some(AST.If(_, _, _)) => i2 - i1 match {
      case 1 => Some("then")
      case _ => Some("else")
    }
    case Some(AST.While(_, _)) => i2 - i1 match {
      case 1 => Some("do")
      case _ => Some("else")
    }
    case _ => None
  }
}
