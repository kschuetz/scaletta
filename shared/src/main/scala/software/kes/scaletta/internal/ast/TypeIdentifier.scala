package software.kes.scaletta.internal.ast

import software.kes.scaletta.internal.types.ConjunctionType
import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

sealed trait TypeIdentifier[F[_]] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): TypeIdentifier[G]
}

object TypeIdentifier {
  def name[F[_]](name: F[Identifier[F]]): TypeIdentifier[F] = Name(name)

  def select[F[_]](qualifier: F[TypeIdentifier[F]], name: F[Identifier[F]]): TypeIdentifier[F] =
    Select(qualifier, name)

  def applied[F[_]](qualifier: F[TypeIdentifier[F]],
                    args: F[TypeIdentifier[F]]*)(implicit F: Functor[F]): TypeIdentifier[F] =
    args.toList match {
      case Nil =>
        qualifier match {
          case q: TypeIdentifier[F @unchecked] => q
          case _ =>
            // This case handles when F[TypeIdentifier[F]] is not TypeIdentifier[F] (e.g. Pos[TypeIdentifier[Pos]])
            // In that case, we can't easily extract the value without knowing F.
            // But for the sake of the factory method's traditional behavior:
            throw new IllegalArgumentException("applied with zero arguments called on a wrapped qualifier")
        }
      case x :: xs => Applied(qualifier, ::(x, xs))
    }

  def union[F[_]](first: F[TypeIdentifier[F]],
                  second: F[TypeIdentifier[F]],
                  rest: F[TypeIdentifier[F]]*): TypeIdentifier[F] =
    conjunction(ConjunctionType.Union, first, second, rest: _*)

  def intersection[F[_]](first: F[TypeIdentifier[F]],
                         second: F[TypeIdentifier[F]],
                         rest: F[TypeIdentifier[F]]*): TypeIdentifier[F] =
    conjunction(ConjunctionType.Intersection, first, second, rest: _*)

  def function[F[_]](params: Vector[F[TypeIdentifier[F]]],
                     result: F[TypeIdentifier[F]]): TypeIdentifier[F] =
    Function(params, result)

  def tuple[F[_]](elements: Vector[F[TypeIdentifier[F]]]): TypeIdentifier[F] =
    Tuple(elements)

  /** A simple type referred to by name (e.g., `Int`, `String`). */
  case class Name[F[_]](name: F[Identifier[F]]) extends TypeIdentifier[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Name[G] =
      Name(phi(F.map(name)(_.mapK(phi))))
  }

  /** A qualified type (e.g., `foo.Bar`). */
  case class Select[F[_]](qualifier: F[TypeIdentifier[F]], name: F[Identifier[F]]) extends TypeIdentifier[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Select[G] =
      Select(phi(F.map(qualifier)(_.mapK(phi))), phi(F.map(name)(_.mapK(phi))))
  }

  /**
   * A type with arguments (e.g., `List[Int]`).
   */
  case class Applied[F[_]](qualifier: F[TypeIdentifier[F]], args: ::[F[TypeIdentifier[F]]]) extends TypeIdentifier[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Applied[G] = {
      val newArgs = args.map(a => phi(F.map(a)(_.mapK(phi))))
      Applied(
        phi(F.map(qualifier)(_.mapK(phi))),
        ::(newArgs.head, newArgs.tail)
      )
    }
  }

  /**
   * A function type (e.g., `(Int, String) => Boolean`).
   */
  case class Function[F[_]](params: Vector[F[TypeIdentifier[F]]],
                            result: F[TypeIdentifier[F]]) extends TypeIdentifier[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Function[G] =
      Function(
        params.map(p => phi(F.map(p)(_.mapK(phi)))),
        phi(F.map(result)(_.mapK(phi)))
      )
  }

  /**
   * A union or intersection type (e.g., `A | B` or `A & B`).
   * Components are stored in a Vector to preserve order and duplicates as written in source.
   */
  case class Conjunction[F[_]](conjunctionType: ConjunctionType,
                               components: Vector[F[TypeIdentifier[F]]]) extends TypeIdentifier[F] {
    override def toString: String = components.mkString(s" ${conjunctionType.operator} ")

    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Conjunction[G] =
      Conjunction(
        conjunctionType,
        components.map(c => phi(F.map(c)(_.mapK(phi))))
      )
  }

  /**
   * A tuple type (e.g., `(Int, String)`).
   */
  case class Tuple[F[_]](elements: Vector[F[TypeIdentifier[F]]]) extends TypeIdentifier[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Tuple[G] =
      Tuple(elements.map(e => phi(F.map(e)(_.mapK(phi)))))
  }

  implicit object typeIdentifierFunctorK extends FunctorK[TypeIdentifier] {
    def mapK[F[_], G[_]](tf: TypeIdentifier[F])(nt: F ~> G)(implicit F: Functor[F]): TypeIdentifier[G] =
      tf.mapK(nt)
  }

  private def conjunction[F[_]](conjunctionType: ConjunctionType,
                                first: F[TypeIdentifier[F]],
                                second: F[TypeIdentifier[F]],
                                rest: F[TypeIdentifier[F]]*): TypeIdentifier[F] = {
    Conjunction(conjunctionType, first +: second +: rest.toVector)
  }
}
