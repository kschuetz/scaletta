package software.kes.scaletta.ast

import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.types.ConjunctionType

sealed trait TypeIdentifier

object TypeIdentifier {
  def name(name: Identifier[Pos]): TypeIdentifier = Name(name)

  def applied(name: Identifier[Pos],
              args: TypeIdentifier*): TypeIdentifier = {
    args.toList match {
      case Nil => Name(name)
      case x :: xs => Applied(name, ::(x, xs))
    }
  }

  def union(first: TypeIdentifier,
            second: TypeIdentifier,
            rest: TypeIdentifier*): TypeIdentifier =
    conjunction(ConjunctionType.Union, first, second, rest: _*)

  def intersection(first: TypeIdentifier,
                   second: TypeIdentifier,
                   rest: TypeIdentifier*): TypeIdentifier =
    conjunction(ConjunctionType.Intersection, first, second, rest: _*)

  def function(params: Vector[TypeIdentifier],
               result: TypeIdentifier): TypeIdentifier =
    Function(params, result)

  /** A simple type referred to by name (e.g., `Int`, `String`). */
  case class Name(name: Identifier[Pos]) extends TypeIdentifier

  /**
   * A type with arguments (e.g., `List[Int]`).
   */
  case class Applied(name: Identifier[Pos], args: ::[TypeIdentifier]) extends TypeIdentifier

  /**
   * A function type (e.g., `(Int, String) => Boolean`).
   */
  case class Function(params: Vector[TypeIdentifier],
                      result: TypeIdentifier) extends TypeIdentifier

  /**
   * A union type (e.g., `A | B`).
   * Components are stored in a Set to ensure order-insensitivity and uniqueness.
   * Logic should ensure the set contains at least two elements.
   */
  object Conjunction {
    def unapply(arg: Conjunction): Option[(ConjunctionType, Set[TypeIdentifier])] =
      Some((arg.conjunctionType, arg.components))
  }

  final class Conjunction private[TypeIdentifier](val conjunctionType: ConjunctionType,
                                                  val components: Set[TypeIdentifier]) extends TypeIdentifier {
    override def equals(other: Any): Boolean = other match {
      case that: Conjunction =>
        conjunctionType == that.conjunctionType &&
          components == that.components
      case _ => false
    }

    override def hashCode(): Int = {
      Seq(conjunctionType.hashCode(), components.hashCode())
        .foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString: String = components.mkString(s" ${conjunctionType.operator} ")
  }

  private def conjunction(conjunctionType: ConjunctionType,
                          first: TypeIdentifier,
                          second: TypeIdentifier,
                          rest: TypeIdentifier*): TypeIdentifier = {
    def combine(acc: Set[TypeIdentifier], ti: TypeIdentifier): Set[TypeIdentifier] =
      ti match {
        case Conjunction(ct, identifiers) if ct == conjunctionType =>
          identifiers.foldLeft(acc) {
            case (acc1, identifier) => combine(acc1, identifier)
          }
        case other => acc + other
      }


    val step1 = combine(Set.empty, first)
    val step2 = combine(step1, second)
    val components = rest.foldLeft(step2) {
      case (acc, ti) => combine(acc, ti)
    }
    if (components.size >= 2) new Conjunction(conjunctionType, components) else components.head
  }
}
