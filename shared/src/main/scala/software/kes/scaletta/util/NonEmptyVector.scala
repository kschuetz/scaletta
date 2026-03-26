package software.kes.scaletta.util

object NonEmptyVector {
  def apply[A](first: A, rest: A*): NonEmptyVector[A] =
    new NonEmptyVector(first +: rest.toVector)

  def tryFrom[A](elems: Iterable[A]): Option[NonEmptyVector[A]] =
    elems match {
      case x: NonEmptyVector[A @unchecked] => Some(x)
      case other if other.nonEmpty => Some(new NonEmptyVector(other.toVector))
      case _ => None
    }

  /**
   * Creates a NonEmptyVector from an Iterable.
   * Throws an exception if the Iterable is empty.
   */
  def from[A](elems: Iterable[A]): NonEmptyVector[A] =
    tryFrom(elems)
      .getOrElse(throw new IllegalArgumentException("Vector must be non-empty"))
}

final class NonEmptyVector[+A] private(val underlying: Vector[A]) extends IndexedSeq[A] {
  override def apply(index: Int): A = underlying(index)

  override def length: Int = underlying.length

  override def iterator: Iterator[A] = underlying.iterator

  override def isEmpty: Boolean = false

  override def head: A = underlying.head

  override def last: A = underlying.last

  def prepend[B >: A](elem: B): NonEmptyVector[B] =
    new NonEmptyVector(elem +: underlying)

  def append[B >: A](elem: B): NonEmptyVector[B] =
    new NonEmptyVector(underlying :+ elem)

  def concatNE[B >: A](suffix: IterableOnce[B]): NonEmptyVector[B] =
    new NonEmptyVector(underlying ++ suffix)

  override def updated[B >: A](index: Int, elem: B): NonEmptyVector[B] =
    new NonEmptyVector(underlying.updated(index, elem))

  override def map[B](f: A => B): NonEmptyVector[B] =
    new NonEmptyVector(underlying.map(f))

  override def filter(p: A => Boolean): Vector[A] = underlying.filter(p)

  override def tail: Vector[A] = underlying.tail

  override def init: Vector[A] = underlying.init

  override def drop(n: Int): Vector[A] = underlying.drop(n)

  override def take(n: Int): Vector[A] = underlying.take(n)

  override def toVector: Vector[A] = underlying

  override def equals(other: Any): Boolean = {
    other match {
      case that: NonEmptyVector[_] => this.underlying == that.underlying
      case that: Vector[_] => this.underlying == that
      case _ => false
    }
  }

  override def hashCode(): Int = underlying.hashCode()

  override def toString(): String = s"NonEmpty${underlying.toString()}"
}
