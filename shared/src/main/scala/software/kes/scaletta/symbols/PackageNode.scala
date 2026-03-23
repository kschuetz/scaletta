package software.kes.scaletta.symbols

import software.kes.scaletta.common.{PackagePath, PackageSegment}

private[symbols] object PackageNode {
  def empty[A]: PackageNode[A] = new PackageNode(Map.empty, Map.empty)
}

/**
 * A node in the package tree.
 *
 * @param symbols     Symbols defined directly in this package.
 * @param subpackages Nested packages.
 * @tparam A The type of value stored in the node.
 */
private[symbols] final class PackageNode[A](val symbols: Map[Name, A],
                                            val subpackages: Map[String, PackageNode[A]]) {

  /**
   * Looks up a symbol directly in this package.
   */
  def get(identifier: Name): Option[A] = symbols.get(identifier)

  /**
   * Returns the sub-package node for the given segment, if it exists.
   */
  def getSubpackage(segment: PackageSegment): Option[PackageNode[A]] =
    subpackages.get(segment.name)

  /**
   * Traverses the tree to find the node at the given absolute path.
   */
  def findNode(path: PackagePath.Absolute): Option[PackageNode[A]] =
    findNode(path.components)

  /**
   * Traverses the tree to find the node at the given relative path starting from this node.
   */
  def findNode(path: PackagePath.Relative): Option[PackageNode[A]] =
    findNode(path.components)

  private def findNode(components: Vector[PackageSegment]): Option[PackageNode[A]] = {
    def go(current: PackageNode[A], index: Int): Option[PackageNode[A]] = {
      if (index >= components.length) {
        Some(current)
      } else {
        current.getSubpackage(components(index)).flatMap(go(_, index + 1))
      }
    }

    go(this, 0)
  }

  /**
   * Adds a symbol at the given absolute path, creating nested nodes as needed.
   */
  def add(path: PackagePath.Absolute, identifier: Name, value: A): PackageNode[A] = {
    val components = path.components

    def go(current: PackageNode[A], index: Int): PackageNode[A] = {
      if (index >= components.length) {
        new PackageNode(current.symbols + (identifier -> value), current.subpackages)
      } else {
        val head = components(index)
        val sub = current.subpackages.getOrElse(head.name, PackageNode.empty[A])
        val newSub = go(sub, index + 1)
        new PackageNode(current.symbols, current.subpackages + (head.name -> newSub))
      }
    }

    go(this, 0)
  }

  /**
   * Merges another tree into this one.
   * Symbols in the other tree will overwrite symbols with the same name in this tree.
   */
  def merge(other: PackageNode[A]): PackageNode[A] = {
    val mergedSymbols = symbols ++ other.symbols
    val mergedSubPackages = other.subpackages.foldLeft(subpackages) {
      case (acc, (name, otherSub)) =>
        val thisSub = acc.getOrElse(name, PackageNode.empty[A])
        acc + (name -> thisSub.merge(otherSub))
    }
    new PackageNode(mergedSymbols, mergedSubPackages)
  }

  override def equals(other: Any): Boolean = other match {
    case that: PackageNode[_] =>
      symbols == that.symbols && subpackages == that.subpackages
    case _ => false
  }

  override def hashCode(): Int = {
    31 * symbols.hashCode() + subpackages.hashCode()
  }
}

