package software.kes.scaletta.util

sealed class NonEmptySet[A](underlying: Set[A]) extends Set[A] {
  def incl(elem: A): NonEmptySet[A] =
    new NonEmptySet(underlying.incl(elem))

  def excl(elem: A): Set[A] = {
    val updated = underlying.excl(elem)
    if (updated eq underlying) this
    else if (updated.isEmpty) underlying
    else new NonEmptySet(updated)
  }

  def contains(elem: A): Boolean = underlying.contains(elem)

  def iterator: Iterator[A] = underlying.iterator

  def ++(rhs: Set[A]): NonEmptySet[A] = {
    val updated = underlying ++ rhs
    if (updated eq underlying) this
    else new NonEmptySet(updated)
  }

  override def isEmpty: Boolean = false
}

sealed class SetTwoPlus[A](underlying: Set[A]) extends NonEmptySet[A](underlying) {
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

}
