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
   * A union or intersection type (e.g., `A | B` or `A & B`).
   * Components are stored in a Vector to preserve order and duplicates as written in source.
   */
  case class Conjunction(conjunctionType: ConjunctionType,
                         components: Vector[TypeIdentifier]) extends TypeIdentifier {
    override def toString: String = components.mkString(s" ${conjunctionType.operator} ")
  }

  private def conjunction(conjunctionType: ConjunctionType,
                          first: TypeIdentifier,
                          second: TypeIdentifier,
                          rest: TypeIdentifier*): TypeIdentifier = {
    Conjunction(conjunctionType, first +: second +: rest.toVector)
  }
}
