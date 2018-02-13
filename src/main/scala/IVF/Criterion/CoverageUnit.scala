package IVF.Criterion

/**
  * Represents a unit of coverage (for instance node, or path)
  *
  * @see [[Path]]
  * @see [[Node]]
  * @see [[SourceTarget]]
  * @see [[SourceTargets]]
  */
sealed abstract class CoverageUnit

/**
  * The unit is covered if the path is visited
  * @param path path
  */
case class Path(path: Vector[Int]) extends CoverageUnit {
  override def toString: String = {
    path.mkString("~>")
  }

  def contains(path2: Path): Boolean = path.containsSlice(path2.path)
}

/**
  * The unit is covered if ANY path goes through source then through target
  * @param source source
  * @param target target
  */

case class SourceTarget(source: Option[Int], target: Option[Int]) extends CoverageUnit {
  override def toString: String = source.map(_.toString).getOrElse("") + "~~>" + target.map(_.toString).getOrElse("")
}

/**
  * The unit is covered in ANY path goes through source then through ANY target
  * @param source source
  * @param targets list of possible targets
  */

case class SourceTargets(source: Option[Int], targets: Set[Int]) extends CoverageUnit {
  override def toString: String = s"${source.map(_.toString).getOrElse("")}~~>{${targets.mkString(", ")}}"
}

/**
  * The unit is covered if the node is visited
  * @param node node label
  */
case class Node(node: Int) extends CoverageUnit {
  override def toString: String = node.toString
}
