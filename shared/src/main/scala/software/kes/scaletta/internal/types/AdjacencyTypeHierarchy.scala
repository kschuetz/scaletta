package software.kes.scaletta.internal.types

import software.kes.scaletta.api.Type

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object AdjacencyTypeHierarchy {
  /**
   * Creates an empty [[AdjacencyTypeHierarchy]].
   */
  def empty[T]: AdjacencyTypeHierarchy[T] = new AdjacencyTypeHierarchy(Map.empty, Set.empty)

  /**
   * Creates an [[AdjacencyTypeHierarchy]] from the given supertype mappings.
   */
  def fromMap[T](supertypes: Map[Type[T], Set[Type[T]]],
                 valueTypes: Set[Type.Nominal[T]]): AdjacencyTypeHierarchy[T] =
    new AdjacencyTypeHierarchy(supertypes, valueTypes)
}

/**
 * A concrete implementation of [[TypeHierarchy]] that uses an adjacency map
 * to represent the inheritance graph.
 *
 * @tparam T The type of the identifier used for types (e.g., [[TypeId]]).
 */
final class AdjacencyTypeHierarchy[T] private(private val supertypes: Map[Type[T], Set[Type[T]]],
                                              private val valueTypes: Set[Type.Nominal[T]]) extends TypeHierarchy[T] {

  def relationshipFor(lhs: Type[T], rhs: Type[T]): TypeRelationship[T] = {
    if (lhs == rhs) {
      TypeRelationship.Same
    } else if (isSubtype(lhs, rhs)) {
      TypeRelationship.StrictSubtype
    } else if (isSubtype(rhs, lhs)) {
      TypeRelationship.StrictSupertype
    } else {
      val common = findCommonSupertype(lhs, rhs)
      val isC1 = lhs.isInstanceOf[Type.Constructor[_]] || lhs.isInstanceOf[Type.Applied[_]]
      val isC2 = rhs.isInstanceOf[Type.Constructor[_]] || rhs.isInstanceOf[Type.Applied[_]]

      if (isC1 && isC2) {
        common.filterNot(_ == Type.Top).fold[TypeRelationship[T]](TypeRelationship.Unrelated)(TypeRelationship.HaveCommonSupertype(_))
      } else {
        common.fold[TypeRelationship[T]](TypeRelationship.Unrelated)(TypeRelationship.HaveCommonSupertype(_))
      }
    }
  }

  def immediateSupertypes(t: Type[T]): Iterable[Type[T]] =
    t match {
      case Type.Top => Iterable.empty
      case Type.TopValue => Iterable(Type.Top)
      case Type.TopRef => Iterable(Type.Top)
      case c: Type.Constructor[T] => Iterable(Type.Top)
      case _ => supertypes.getOrElse(t, Set.empty)
    }

  /**
   * Checks if `lhs` is a subtype of `rhs`.
   */
  def isSubtype(lhs: Type[T], rhs: Type[T]): Boolean = {
    if (rhs == Type.Top) {
      true
    } else if (rhs == Type.TopValue) {
      lhs match {
        case Type.Bottom => true
        case Type.TopValue => true
        case Type.Unit => true
        case t: Type.Nominal[T] => valueTypes.contains(t)
        case _ => false
      }
    } else if (rhs == Type.TopRef) {
      lhs match {
        case Type.Bottom | Type.BottomRef | Type.TopRef => true
        case _: Type.Function[T] | _: Type.Tuple[T] | _: Type.Constructor[T] => true
        case u: Type.Union[T] => u.types.forall(t => isSubtype(t, rhs))
        case i: Type.Intersection[T] => i.types.exists(t => isSubtype(t, rhs))
        case t: Type.Nominal[T] => !valueTypes.contains(t)
        case _ => false
      }
    } else if (lhs == Type.Bottom) {
      true
    } else if (lhs == Type.BottomRef) {
      rhs match {
        case Type.Bottom => false
        case t: Type.Nominal[T] => !valueTypes.contains(t)
        case _ => true
      }
    } else {
      (lhs, rhs) match {
        case (u: Type.Union[T], _) =>
          u.types.forall(t => isSubtype(t, rhs))
        case (_, i: Type.Intersection[T]) =>
          i.types.forall(t => isSubtype(lhs, t))
        case (i: Type.Intersection[T], _) =>
          i.types.exists(t => isSubtype(t, rhs)) || bfsSubtypeCheck(lhs, rhs)
        case (_, u: Type.Union[T]) =>
          u.types.exists(t => isSubtype(lhs, t)) || bfsSubtypeCheck(lhs, rhs)
        case (Type.Function(p1, r1), Type.Function(p2, r2)) =>
          p1.size == p2.size &&
            p1.zip(p2).forall { case (a, b) => isSubtype(b, a) } && // Contravariant
            isSubtype(r1, r2) // Covariant
        case (Type.Tuple(e1), Type.Tuple(e2)) =>
          e1.size == e2.size &&
            e1.zip(e2).forall { case (a, b) => isSubtype(a, b) } // Covariant
        case (a: Type.Applied[T] @unchecked, c2: Type.Constructor[T] @unchecked) =>
          isSubtype(a.constructor, c2)
        case _ =>
          bfsSubtypeCheck(lhs, rhs)
      }
    }
  }

  private def bfsSubtypeCheck(lhs: Type[T], rhs: Type[T]): Boolean = {
    @tailrec
    def go(queue: Queue[Type[T]], visited: Set[Type[T]]): Boolean = {
      queue.dequeueOption match {
        case Some((current, rest)) =>
          if (current == rhs) {
            true
          } else {
            val nextSupertypes = immediateSupertypes(current).filterNot(visited.contains)
            go(rest.enqueueAll(nextSupertypes), visited ++ nextSupertypes)
          }
        case None => false
      }
    }

    go(Queue(lhs), Set(lhs))
  }

  /**
   * Finds the least upper bound (LUB) of two types.
   * Currently, it returns the first common supertype found during BFS.
   */
  private def findCommonSupertype(lhs: Type[T], rhs: Type[T]): Option[Type[T]] = {
    val lhsAncestors = allAncestors(lhs)

    @tailrec
    def go(queue: Queue[Type[T]], visited: Set[Type[T]]): Option[Type[T]] = {
      queue.dequeueOption match {
        case Some((current, rest)) =>
          if (lhsAncestors.contains(current)) {
            Some(current)
          } else {
            val nextSupertypes = immediateSupertypes(current).filterNot(visited.contains)
            go(rest.enqueueAll(nextSupertypes), visited ++ nextSupertypes)
          }
        case None => None
      }
    }

    go(Queue(rhs), Set(rhs))
  }

  private def allAncestors(t: Type[T]): Set[Type[T]] = {
    @tailrec
    def go(queue: Queue[Type[T]], visited: Set[Type[T]], acc: Set[Type[T]]): Set[Type[T]] = {
      queue.dequeueOption match {
        case Some((current, rest)) =>
          val nextSupertypes = immediateSupertypes(current).filterNot(visited.contains)
          go(rest.enqueueAll(nextSupertypes), visited ++ nextSupertypes, acc + current)
        case None => acc
      }
    }

    go(Queue(t), Set(t), Set.empty)
  }

}
