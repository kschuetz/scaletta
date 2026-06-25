package software.kes.scaletta.internal.types

import software.kes.scaletta.api.Type

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object AdjacencyTypeHierarchy {
  /**
   * Creates an empty [[AdjacencyTypeHierarchy]].
   */
  def empty[T]: AdjacencyTypeHierarchy[T] = new AdjacencyTypeHierarchy(Map.empty)

  /**
   * Creates an [[AdjacencyTypeHierarchy]] from the given supertype mappings.
   */
  def fromMap[T](supertypes: Map[Type[T], Set[Type[T]]]): AdjacencyTypeHierarchy[T] =
    new AdjacencyTypeHierarchy(supertypes)
}

/**
 * A concrete implementation of [[TypeHierarchy]] that uses an adjacency map
 * to represent the inheritance graph.
 *
 * @tparam T The type of the identifier used for types (e.g., [[TypeId]]).
 */
final class AdjacencyTypeHierarchy[T] private(private val supertypes: Map[Type[T], Set[Type[T]]]) extends TypeHierarchy[T] {

  def relationshipFor(lhs: Type[T], rhs: Type[T]): TypeRelationship[T] = {
    if (lhs == rhs) {
      TypeRelationship.Same
    } else if (isSubtypeOf(lhs, rhs)) {
      TypeRelationship.StrictSubtype
    } else if (isSubtypeOf(rhs, lhs)) {
      TypeRelationship.StrictSupertype
    } else {
      findCommonSupertype(lhs, rhs).fold[TypeRelationship[T]](TypeRelationship.Unrelated)(TypeRelationship.HaveCommonSupertype(_))
    }
  }

  def immediateSupertypes(t: Type[T]): Iterable[Type[T]] =
    supertypes.getOrElse(t, Set.empty)

  /**
   * Checks if `lhs` is a subtype of `rhs`.
   */
  private def isSubtypeOf(lhs: Type[T], rhs: Type[T]): Boolean = {
    if (lhs == Type.Bottom) {
      true
    } else if (lhs == Type.BottomRef) {
      // TODO: Handle BottomRef O(1) subtype check for AnyRef
      // For now, it will fall back to the BFS logic if not handled here
      bfsSubtypeCheck(lhs, rhs)
    } else {
      bfsSubtypeCheck(lhs, rhs)
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
