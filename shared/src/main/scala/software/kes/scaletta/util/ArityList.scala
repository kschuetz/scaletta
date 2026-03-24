package software.kes.scaletta.util

object ArityList {
  def apply[A](items: List[A]): ArityList[A] =
    fromSeq(items)

  def of[A](items: A*): ArityList[A] =
    fromSeq(items)

  def fromSeq[A](items: Seq[A]): ArityList[A] =
    NonEmptyArityList.tryFrom(items).getOrElse(EmptyArityList)

  def empty[A]: ArityList[A] = EmptyArityList

  def unapplySeq[A](arg: ArityList[A]): Option[Seq[A]] =
    Some(arg.items)
}

sealed trait ArityList[+A] {
  def items: List[A]

  def arity: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def sameArityAs[B](other: ArityList[B]): Boolean =
    this.arity == other.arity

  def map[B](fn: A => B): ArityList[B]

  def foreach[U](fn: A => U): Unit

  def forall(p: A => Boolean): Boolean

  def exists(p: A => Boolean): Boolean

  def zip[B](other: ArityList[B]): ArityList[(A, B)]

  def prepend[AA >: A](element: AA): NonEmptyArityList[AA]
}

object EmptyArityList extends ArityList[Nothing] {
  def items: List[Nothing] = Nil

  def arity: Int = 0

  def isEmpty: Boolean = true

  def map[B](fn: Nothing => B): ArityList[B] = this

  def foreach[U](fn: Nothing => U): Unit = ()

  def forall(p: Nothing => Boolean): Boolean = true

  def exists(p: Nothing => Boolean): Boolean = false

  def zip[B](other: ArityList[B]): ArityList[(Nothing, B)] = this

  def prepend[AA >: Nothing](element: AA): NonEmptyArityList[AA] =
    NonEmptyArityList.of(element)
}

object NonEmptyArityList {
  def of[A](first: A, rest: A*): NonEmptyArityList[A] =
    new NonEmptyArityList(::(first, rest.toList), rest.length + 1)

  def tryFrom[A](items: Iterable[A]): Option[NonEmptyArityList[A]] = {
    items match {
      case Nil => None
      case list: List[A @unchecked] => Some(new NonEmptyArityList(list, list.length))
      case other =>
        val size = other.size
        if (size == 0) None
        else Some(new NonEmptyArityList(other.toList, size))
    }
  }

  /**
   * Creates a NonEmptyArityList from an iterable.
   * Throws an exception if the iterable is empty.
   */
  def from[A](items: Iterable[A]): NonEmptyArityList[A] =
    tryFrom(items).getOrElse(sys.error(s"Cannot create NonEmptyArityList from empty iterable"))

  def unapply[A](arg: NonEmptyArityList[A]): Option[(A, ArityList[A])] =
    Some((arg.head, arg.tail))
}

final class NonEmptyArityList[A] private(val items: List[A],
                                         val arity: Int) extends ArityList[A] {
  def isEmpty: Boolean = false

  def map[B](fn: A => B): NonEmptyArityList[B] =
    new NonEmptyArityList(items.map(fn), arity)

  def foreach[U](fn: A => U): Unit = items.foreach(fn)

  def forall(p: A => Boolean): Boolean = items.forall(p)

  def exists(p: A => Boolean): Boolean = items.exists(p)

  def zip[B](other: ArityList[B]): ArityList[(A, B)] = {
    val newItems = items.zip(other.items)
    NonEmptyArityList.tryFrom(newItems).getOrElse(EmptyArityList)
  }

  def head: A = items.head

  def tail: ArityList[A] = items.tail match {
    case Nil => EmptyArityList
    case t => new NonEmptyArityList(t, arity - 1)
  }

  def prepend[AA >: A](element: AA): NonEmptyArityList[AA] =
    new NonEmptyArityList(element :: items, arity + 1)

  override def equals(other: Any): Boolean = other match {
    case that: NonEmptyArityList[_] =>
      arity == that.arity &&
        items == that.items
    case _ => false
  }

  override def hashCode(): Int =
    items.hashCode()
}
