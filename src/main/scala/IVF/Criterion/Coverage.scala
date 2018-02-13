package IVF.Criterion

import IVF.Model.{CFG, State}

/**
  * Enables to test a coverage
  *
  * It is returned by a criterion build function
  */
sealed trait Coverage {
  /**
    * Tests if a list of states covers what is required when executing a control flow graph
    *
    * @param cfg    control flow graph
    * @param states initial states
    * @return the list of uncovered units
    */
  def coverTest(cfg: CFG, states: List[State]): Seq[CoverageUnit]

  /**
    * Get a human readable message of coverage success or not
    *
    * @param cfg    control flow graph
    * @param states initial states
    * @return human readable message including uncovered units
    */
  def coverTestString(cfg: CFG, states: List[State]): String
}

/**
  * Verifies if a set of path is covered
  *
  * @param paths paths to cover
  */
case class PathCoverage(paths: Set[Path]) extends Coverage {
  override def coverTest(cfg: CFG, states: List[State]): Seq[Path] = {
    // first we look at what paths we traversed when executing the cfg over the different states
    val traversedPaths = states.map(state =>
      // this uses the traversable inheritance in an elegant manner
      Path((Vector[Int]() /: cfg.exec(0, state)) {
        case (accV, node) => accV :+ node.value
      })
    )

    // then for every required path
    paths
      .map(path =>
        traversedPaths
          // we check if any traversed path contains the required path
          .find(_.contains(path)) match {
          // and we return the bad boys
          case Some(_) => None
          case None => Some(path)
        })
      // and we filter out every covered path
      .filter(_.nonEmpty)
      .flatten.toSeq
  }

  override def coverTestString(cfg: CFG, states: List[State]): String = {
    val paths = this.coverTest(cfg, states)
    if (paths.nonEmpty)
      s"paths {${paths.mkString(", ")}} not traversed"
    else
      "all required paths traversed"
  }

  override def toString: String = s"{${paths.mkString(", ")}}"
}

/**
  * Verifies if a set of source target tuples is covered following the principle :
  * at least one path must go from source to target
  *
  * @param st source target tuples
  */
case class SourceTargetCoverage(st: Set[SourceTarget]) extends Coverage {
  override def coverTest(cfg: CFG, states: List[State]): Seq[SourceTarget] = {
    // first we look at what paths we traversed when executing the cfg over the different states
    val traversedPaths = states.map(state =>
      Path((Vector[Int]() /: cfg.exec(0, state)) {
        case (accV, node) => accV :+ node.value
      })
    )

    // then for every source target tuple
    st.map {
      case p@SourceTarget(Some(source), Some(target)) =>
        // if we find any valid traversed path
        traversedPaths.find(path => {
          val sourceIndexInPath = path.path.indexOf(source)
          if (sourceIndexInPath >= 0) {
            // (valid means here that source is in path before target and both are in path)
            val targetIndexInPath = path.path.indexOf(target, sourceIndexInPath + 1)
            targetIndexInPath >= 0
          }
          else false
        }) match {
          // we return none when everything is ok, and the missing st tuple else
          case Some(_) => None
          case None => Some(p)
        }
      case p => Some(p)
    }
      // we just keep invalid tuples
      .filter(_.nonEmpty)
      .flatten.toSeq
  }

  override def toString: String = "paths: {" + st.mkString(", ") + "}"

  override def coverTestString(cfg: CFG, states: List[State]): String = {
    val sts = this.coverTest(cfg, states)
    if (sts.nonEmpty)
      s"source target paths {${sts.mkString(", ")}} not traversed"
    else
      s"all required source target paths found"
  }
}

/**
  * Verifies if a set of source targetS tuples is covered following the principle :
  * at least one path must go from source to any of the target of the tuple
  *
  * @param stss source targets tuples
  */
case class SourceAnyTargetCoverage(stss: Set[SourceTargets]) extends Coverage {
  override def coverTest(cfg: CFG, states: List[State]): Seq[SourceTargets] = {
    // here we reuse the previous coverage
    stss
      .map(sts => {
        val stList = sts.targets.map(tgt => SourceTarget(Some(sts.source.get), Some(tgt)))
        val stCovList = stList.map(st => SourceTargetCoverage(Set(st)))
        if (sts.targets.isEmpty) {
          Some(sts)
          // here we test that at least one of the sub ST tuple is not problematic
        } else if (stCovList.map(_.coverTest(cfg, states)).exists(_.isEmpty)) {
          None
        } else {
          Some(sts)
        }
      }
      )
      .filter(_.nonEmpty)
      .flatten.toSeq
  }

  override def coverTestString(cfg: CFG, states: List[State]): String = {
    val failedStss = this.coverTest(cfg, states)
    if (failedStss.nonEmpty) {
      s"source to any target paths {${failedStss.mkString(", ")}} not traversed"
    } else
      s"all sources to any target paths traversed"
  }

  override def toString: String = "paths: {" + stss.mkString(", ") + "}"
}

/**
  * Verifies if a set of nodes is covered by the executions of cfg over different states
  * @param nodes nodes to cover
  */
case class NodeCoverage(nodes: Set[Node]) extends Coverage {
  override def coverTest(cfg: CFG, states: List[State]): Seq[Node] = {
    // here we get all visited paths
    val traversedPaths = states.map(state =>
      (Vector[Node]() /: cfg.exec(0, state)) {
        case (accV, node) => accV :+ Node(node.value)
      }
    )

    // just shrink that to a list of nodes
    val traversedNodes = traversedPaths.flatten

    // for every node
    nodes
      .map(
        node =>
          // check that the node is present in the visited nodes
          if (traversedNodes.contains(node)) None
          else Some(node)
      )
      .filter(_.nonEmpty)
      .flatten.toSeq
  }

  override def toString: String = s"nodes : {${nodes.mkString(", ")}}"

  override def coverTestString(cfg: CFG, states: List[State]): String = {
    val nodes = this.coverTest(cfg, states)
    if (nodes.nonEmpty) s"nodes {${nodes.mkString(", ")}} not traversed"
    else
      s"all required nodes traversed"
  }
}

/**
  * Utils
  */
object Coverage {
  def coverageAsSeq(coverage: Coverage): Seq[CoverageUnit] = coverage match {
    case NodeCoverage(set) => set.toSeq
    case PathCoverage(set) => set.toSeq
    case SourceTargetCoverage(set) => set.toSeq
    case SourceAnyTargetCoverage(set) => set.toSeq
  }
}