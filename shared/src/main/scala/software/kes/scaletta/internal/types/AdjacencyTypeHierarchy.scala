package software.kes.scaletta.internal.types

import software.kes.scaletta.api.{ProperType, Type, Variance}
import software.kes.scaletta.util.{NonEmptyVector, SetTwoPlus, VectorTwoPlus}

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
    } else {
      val lSubR = isSubtype(lhs, rhs)
      val rSubL = isSubtype(rhs, lhs)
      if (lSubR && rSubL) {
        TypeRelationship.Same
      } else if (lSubR) {
        TypeRelationship.StrictSubtype
      } else if (rSubL) {
        TypeRelationship.StrictSupertype
      } else {
        val lub = leastUpperBound(lhs, rhs)
        TypeRelationship.HaveCommonSupertype(lub)
      }
    }
  }

  def immediateSupertypes(t: Type[T]): Iterable[Type[T]] =
    t match {
      case Type.Top => Iterable.empty
      case Type.TopValue => Iterable(Type.Top)
      case Type.TopRef => Iterable(Type.Top)
      case _: Type.Constructor[T] => Iterable(Type.TopRef)
      case a: Type.Applied[T] =>
        val constructorSupertypes = supertypes.getOrElse(a.constructor, Set.empty)
        val fromApplied = constructorSupertypes.collect {
          case c: Type.Constructor[T] => Type.Applied(c, a.arguments)
          case other => other.substitute(a.arguments.map(_.value))
        }
        fromApplied ++ Iterable(a.constructor, Type.TopRef) ++ supertypes.getOrElse(a, Set.empty)
      case _: Type.Function[T] => Iterable(Type.TopRef)
      case _: Type.Tuple[T] => Iterable(Type.TopRef)
      case n: Type.Nominal[T] =>
        val fromMap = supertypes.getOrElse(n, Set.empty)
        if (fromMap.isEmpty) {
          if (valueTypes.contains(n)) Iterable(Type.TopValue)
          else Iterable(Type.TopRef)
        } else {
          fromMap
        }
      case Type.Intersection(types) =>
        types.toVector ++ types.toVector.flatMap(immediateSupertypes)
      case Type.Union(types) =>
        // A | B's supertypes are those that are supertypes of ALL components.
        // For BFS, we can return the intersection of supertypes of components,
        // but that's hard to represent as a simple Iterable.
        // Instead, we return the union of supertypes, which is a bit loose but
        // BFS will eventually find the common ones if they exist.
        types.toVector.flatMap(immediateSupertypes)
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
        case u: Type.Union[T] => u.types.forall(t => isSubtype(t, rhs))
        case i: Type.Intersection[T] => i.types.exists(t => isSubtype(t, rhs))
        case _ => false
      }
    } else if (rhs == Type.TopRef) {
      lhs match {
        case Type.Bottom | Type.BottomRef | Type.TopRef => true
        case _: Type.Function[T] | _: Type.Tuple[T] | _: Type.Constructor[T] | _: Type.Applied[T] => true
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
        case (a1: Type.Applied[T] @unchecked, a2: Type.Applied[T] @unchecked) =>
          if (a1.constructor == a2.constructor && a1.arguments.size == a2.arguments.size) {
            a1.arguments.toVector.zip(a2.arguments.toVector).forall {
              case (arg1, arg2) =>
                arg2.parameter.variance match {
                  case Variance.Invariant => arg1.value == arg2.value
                  case Variance.Covariant => isSubtype(arg1.value, arg2.value)
                  case Variance.Contravariant => isSubtype(arg2.value, arg1.value)
                }
            }
          } else {
            immediateSupertypes(a1).exists(s => isSubtype(s, a2))
          }
        case (a1: Type.Applied[T] @unchecked, a2: Type[T]) if !a2.isInstanceOf[Type.Applied[_]] =>
          bfsSubtypeCheck(a1, a2)
        case (a: Type.Applied[T] @unchecked, c2: Type.Constructor[T] @unchecked) =>
          isSubtype(a.constructor, c2)
        case _ =>
          bfsSubtypeCheck(lhs, rhs)
      }
    }
  }

  private def leastUpperBound(a: Type[T], b: Type[T]): Type[T] = {
    if (a == b) a
    else if (isSubtype(a, b)) b
    else if (isSubtype(b, a)) a
    else {
      (a, b) match {
        case (Type.Top, _) | (_, Type.Top) => Type.Top
        case (Type.Bottom, other) => other
        case (other, Type.Bottom) => other

        case (f1: Type.Function[T], f2: Type.Function[T]) if f1.parameters.size == f2.parameters.size =>
          val p = f1.parameters.zip(f2.parameters).map { case (p1, p2) => greatestLowerBound(p1, p2).asInstanceOf[ProperType[T]] }
          val r = leastUpperBound(f1.result, f2.result).asInstanceOf[ProperType[T]]
          Type.Function(p, r)

        case (t1: Type.Tuple[T], t2: Type.Tuple[T]) if t1.elements.size == t2.elements.size =>
          val e = t1.elements.toVector.zip(t2.elements.toVector).map { case (e1, e2) => leastUpperBound(e1, e2).asInstanceOf[ProperType[T]] }
          Type.Tuple(VectorTwoPlus.from(e))

        case (a1: Type.Applied[T], a2: Type.Applied[T]) if a1.constructor == a2.constructor && a1.arguments.size == a2.arguments.size =>
          val args = a1.arguments.toVector.zip(a2.arguments.toVector).map { case (arg1, arg2) =>
            val newValue = arg1.parameter.variance match {
              case Variance.Invariant => if (arg1.value == arg2.value) arg1.value else null
              case Variance.Covariant => leastUpperBound(arg1.value, arg2.value).asInstanceOf[ProperType[T]]
              case Variance.Contravariant => greatestLowerBound(arg1.value, arg2.value).asInstanceOf[ProperType[T]]
            }
            if (newValue == null) null else arg1.copy(value = newValue)
          }
          if (args.contains(null)) nominalLUB(a1, a2)
          else simplifyType(Type.Applied(a1.constructor, NonEmptyVector.from(args)))

        case (u1: Type.Union[T], _) =>
          val results = u1.types.toVector.map(t => leastUpperBound(t, b).asInstanceOf[ProperType[T]])
          simplifyType(Type.Union(SetTwoPlus.from(results)))
        case (_, u2: Type.Union[T]) =>
          leastUpperBound(b, a)

        case (i1: Type.Intersection[T], _) =>
          val results = i1.types.toVector.map(t => leastUpperBound(t, b).asInstanceOf[ProperType[T]])
          simplifyType(Type.Intersection(SetTwoPlus.from(results)))
        case (_, i2: Type.Intersection[T]) =>
          leastUpperBound(b, a)

        case _ => nominalLUB(a, b)
      }
    }
  }

  private def greatestLowerBound(a: Type[T], b: Type[T]): Type[T] = {
    if (a == b) a
    else if (isSubtype(a, b)) a
    else if (isSubtype(b, a)) b
    else {
      (a, b) match {
        case (Type.Bottom, _) | (_, Type.Bottom) => Type.Bottom
        case (Type.Top, other) => other
        case (other, Type.Top) => other

        case (f1: Type.Function[T], f2: Type.Function[T]) if f1.parameters.size == f2.parameters.size =>
          val p = f1.parameters.zip(f2.parameters).map { case (p1, p2) => leastUpperBound(p1, p2).asInstanceOf[ProperType[T]] }
          val r = greatestLowerBound(f1.result, f2.result).asInstanceOf[ProperType[T]]
          Type.Function(p, r)

        case (t1: Type.Tuple[T], t2: Type.Tuple[T]) if t1.elements.size == t2.elements.size =>
          val e = t1.elements.toVector.zip(t2.elements.toVector).map { case (e1, e2) => greatestLowerBound(e1, e2).asInstanceOf[ProperType[T]] }
          Type.Tuple(VectorTwoPlus.from(e))

        case (a1: Type.Applied[T], a2: Type.Applied[T]) if a1.constructor == a2.constructor && a1.arguments.size == a2.arguments.size =>
          val args = a1.arguments.toVector.zip(a2.arguments.toVector).map { case (arg1, arg2) =>
            val newValue = arg1.parameter.variance match {
              case Variance.Invariant => if (arg1.value == arg2.value) arg1.value else null
              case Variance.Covariant => greatestLowerBound(arg1.value, arg2.value).asInstanceOf[ProperType[T]]
              case Variance.Contravariant => leastUpperBound(arg1.value, arg2.value).asInstanceOf[ProperType[T]]
            }
            if (newValue == null) null else arg1.copy(value = newValue)
          }
          if (args.contains(null)) simplifyType(Type.Intersection(SetTwoPlus(a.asInstanceOf[ProperType[T]], b.asInstanceOf[ProperType[T]])))
          else simplifyType(Type.Applied(a1.constructor, NonEmptyVector.from(args)))

        case (i1: Type.Intersection[T], _) =>
          val results = i1.types.toVector.map(t => greatestLowerBound(t, b).asInstanceOf[ProperType[T]])
          simplifyType(Type.Intersection(SetTwoPlus.from(results)))
        case (_, i2: Type.Intersection[T]) =>
          greatestLowerBound(b, a)

        case (u1: Type.Union[T], _) =>
          val results = u1.types.toVector.map(t => greatestLowerBound(t, b).asInstanceOf[ProperType[T]])
          simplifyType(Type.Union(SetTwoPlus.from(results)))
        case (_, u2: Type.Union[T]) =>
          greatestLowerBound(b, a)

        case (Type.TopRef, _) | (_, Type.TopRef) =>
          if (isSubtype(a, Type.TopRef) && isSubtype(b, Type.TopRef)) Type.intersection(a.asInstanceOf[ProperType[T]], b.asInstanceOf[ProperType[T]])
          else Type.Bottom
        case (Type.TopValue, _) | (_, Type.TopValue) =>
          if (isSubtype(a, Type.TopValue) && isSubtype(b, Type.TopValue)) Type.intersection(a.asInstanceOf[ProperType[T]], b.asInstanceOf[ProperType[T]])
          else Type.Bottom

        case _ => Type.intersection(a.asInstanceOf[ProperType[T]], b.asInstanceOf[ProperType[T]])
      }
    }
  }

  private def nominalLUB(a: Type[T], b: Type[T]): Type[T] = {
    val sA = allAncestors(a)
    val sB = allAncestors(b)

    val commonNominal = sA.intersect(sB)

    val commonApplied = for {
      case1: Type.Applied[T] <- sA.collect { case app: Type.Applied[T] => app }
      case2: Type.Applied[T] <- sB.collect { case app: Type.Applied[T] => app }
      if (case1 != a || case2 != b) && (case1 != b || case2 != a)
      if case1.constructor == case2.constructor && case1.arguments.size == case2.arguments.size
    } yield leastUpperBound(case1, case2)

    val candidates = commonNominal ++ commonApplied
    val minimalCandidates = candidates.foldLeft(Set.empty[Type[T]]) { (acc, curr) =>
      if (acc.exists(t => isSubtype(t, curr))) acc
      else acc.filterNot(t => isSubtype(curr, t)) + curr
    }

    if (minimalCandidates.size == 1) {
      minimalCandidates.head
    } else {
      val properCandidates = minimalCandidates.collect { case p: ProperType[T] => p }.toSet
      if (properCandidates.size >= 2) {
        simplifyType(Type.Intersection(SetTwoPlus.from(properCandidates)))
      } else if (properCandidates.size == 1) {
        properCandidates.head
      } else {
        minimalCandidates.headOption.getOrElse(Type.Top)
      }
    }
  }

  private def simplifyType(t: Type[T]): Type[T] = t match {
    case u: Type.Union[T] =>
      val flattened = flattenUnion(u.types.toSet)
      if (flattened.contains(Type.Top)) Type.Top
      else {
        val simplified = flattened.foldLeft(Set.empty[ProperType[T]]) { (acc, curr) =>
          if (acc.exists(t => isSubtype(curr, t))) acc
          else acc.filterNot(t => isSubtype(t, curr)) + curr
        }
        if (simplified.size == 1) simplified.head
        else Type.Union(SetTwoPlus.from(simplified))
      }
    case i: Type.Intersection[T] =>
      val flattened = flattenIntersection(i.types.toSet)
      if (flattened.contains(Type.Bottom)) Type.Bottom
      else {
        val simplified = flattened.foldLeft(Set.empty[ProperType[T]]) { (acc, curr) =>
          if (acc.exists(t => isSubtype(t, curr))) acc
          else acc.filterNot(t => isSubtype(curr, t)) + curr
        }
        if (simplified.size == 1) simplified.head
        else Type.Intersection(SetTwoPlus.from(simplified))
      }
    case _ => t
  }

  private def flattenUnion(types: Set[ProperType[T]]): Set[ProperType[T]] = {
    types.flatMap {
      case u: Type.Union[T] => flattenUnion(u.types.toSet)
      case Type.Bottom => Set.empty[ProperType[T]]
      case other => Set(other)
    }
  }

  private def flattenIntersection(types: Set[ProperType[T]]): Set[ProperType[T]] = {
    types.flatMap {
      case i: Type.Intersection[T] => flattenIntersection(i.types.toSet)
      case Type.Top => Set.empty[ProperType[T]]
      case other => Set(other)
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
