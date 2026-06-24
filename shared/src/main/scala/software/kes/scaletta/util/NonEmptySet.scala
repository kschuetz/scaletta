package software.kes.scaletta.util

object NonEmptySet {
  def apply[A](elem: A, more: A*): NonEmptySet[A] =
    if (more.nonEmpty) {
      new SetTwoPlus(Set(elem) ++ more)
    } else {
      new NonEmptySet(Set(elem))
    }

  def tryFrom[A](elems: Iterable[A]): Option[NonEmptySet[A]] =
    if (elems.nonEmpty) Some {
      val set = elems.toSet
      if (set.size >= 2) new SetTwoPlus(set)
      else new NonEmptySet(set)
    } else None

  def from[A](elems: Iterable[A]): NonEmptySet[A] =
    tryFrom(elems)
      .getOrElse(throw new IllegalArgumentException("Cannot create NonEmptySet from empty Iterable"))
}

sealed class NonEmptySet[A] private[util](val underlying: Set[A]) extends Set[A] {
  def incl(elem: A): NonEmptySet[A] =
    new NonEmptySet(underlying.incl(elem))

  def excl(elem: A): Set[A] = {
    val updated = underlying.excl(elem)
    if (updated eq underlying) this
    else if (updated.isEmpty) updated
    else new NonEmptySet(updated)
  }

  def contains(elem: A): Boolean = underlying.contains(elem)

  def iterator: Iterator[A] = underlying.iterator

  def ++(rhs: Set[A]): NonEmptySet[A] = {
    val updated = underlying ++ rhs
    if (updated eq underlying) this
    else new NonEmptySet(updated)
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: NonEmptySet[_] => this.underlying == that.underlying
      case that: Set[_] => this.underlying == that
      case _ => false
    }

  override def hashCode(): Int = underlying.hashCode()

  override def toString(): String = s"NonEmptySet(${underlying.mkString(", ")})"
}

object SetTwoPlus {
  def apply[A](elem1: A, elem2: A, more: A*): SetTwoPlus[A] =
    new SetTwoPlus(Set(elem1, elem2) ++ more)

  def tryFrom[A](elems: Iterable[A]): Option[SetTwoPlus[A]] = {
    val iter = elems.iterator
    if (iter.hasNext) {
      iter.next()
      if (iter.hasNext) Some(new SetTwoPlus(elems.toSet))
      else None
    } else None
  }

  /**
   * Creates a SetTwoPlus from an Iterable.
   * Throws an exception if the Iterable has fewer than two elements.
   */
  def from[A](elems: Iterable[A]): SetTwoPlus[A] =
    tryFrom(elems)
      .getOrElse(throw new IllegalArgumentException("Cannot create SetTwoPlus from Iterable with fewer than two elements"))
}

sealed class SetTwoPlus[A] private[util](underlying: Set[A]) extends NonEmptySet[A](underlying) {
  override def incl(elem: A): SetTwoPlus[A] =
    new SetTwoPlus(underlying.incl(elem))

  override def excl(elem: A): NonEmptySet[A] = {
    val updated = underlying.excl(elem)
    if (updated eq underlying) this
    else if (updated.size == 1) new NonEmptySet(updated)
    else new SetTwoPlus(updated)
  }

  override def ++(rhs: Set[A]): SetTwoPlus[A] = {
    val updated = underlying ++ rhs
    if (updated eq underlying) this
    else new SetTwoPlus(updated)
  }

  override def toString(): String = s"SetTwoPlus(${underlying.mkString(", ")})"
}
