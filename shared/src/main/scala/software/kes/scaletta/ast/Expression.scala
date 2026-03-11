package software.kes.scaletta.ast

import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.util.functional.{Functor, ~>}

sealed trait Expression[F[_]] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Expression[G]
}

case class Block[F[_]](declarations: Vector[F[Declaration[F]]],
                       result: F[Expression[F]]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Block[G] =
    Block(
      declarations.map(d => phi(F.map(d)(_.mapK(phi)))),
      phi(F.map(result)(_.mapK(phi)))
    )
}

object Reference {
  def single[F[_]](id: F[Identifier[F]]): Reference[F] =
    Reference(::(id, Nil))
}

case class Reference[F[_]](path: ::[F[Identifier[F]]]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Reference[G] =
    Reference(::(phi(F.map(path.head)(_.mapK(phi))), path.tail.map(id => phi(F.map(id)(_.mapK(phi))))))
}

case class Typed[F[_]](expression: F[Expression[F]],
                       ascription: F[TypeIdentifier[F]]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Typed[G] =
    Typed(
      phi(F.map(expression)(_.mapK(phi))),
      phi(F.map(ascription)(_.mapK(phi)))
    )
}

sealed trait Literal[F[_]] extends Expression[F]

object Literal {
  def int[F[_]](value: Int): Literal[F] = IntLiteral(value)

  def long[F[_]](value: Long): Literal[F] = LongLiteral(value)

  def float[F[_]](value: Float): Literal[F] = FloatLiteral(value)

  def double[F[_]](value: Double): Literal[F] = DoubleLiteral(value)

  def boolean[F[_]](value: Boolean): Literal[F] = if (value) True() else False()

  def true_[F[_]](): Literal[F] = True()

  def false_[F[_]](): Literal[F] = False()

  def null_[F[_]](): Literal[F] = Null()

  def char[F[_]](value: Char): Literal[F] = CharLiteral(value)

  def string[F[_]](value: String): Literal[F] = StringLiteral(value)

  case class IntLiteral[F[_]](value: Int) extends Literal[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): IntLiteral[G] = IntLiteral(value)
  }

  case class LongLiteral[F[_]](value: Long) extends Literal[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): LongLiteral[G] = LongLiteral(value)
  }

  case class FloatLiteral[F[_]](value: Float) extends Literal[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): FloatLiteral[G] = FloatLiteral(value)
  }

  case class DoubleLiteral[F[_]](value: Double) extends Literal[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): DoubleLiteral[G] = DoubleLiteral(value)
  }

  sealed trait BooleanLiteral[F[_]] extends Literal[F] {
    def value: Boolean
  }

  case class True[F[_]]() extends BooleanLiteral[F] {
    def value: Boolean = true

    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): True[G] = True()
  }

  case class False[F[_]]() extends BooleanLiteral[F] {
    def value: Boolean = false

    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): False[G] = False()
  }

  case class Null[F[_]]() extends Literal[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Null[G] = Null()
  }

  case class CharLiteral[F[_]](value: Char) extends Literal[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): CharLiteral[G] = CharLiteral(value)
  }

  case class StringLiteral[F[_]](value: String) extends Literal[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): StringLiteral[G] = StringLiteral(value)
  }
}

case class Tuple[F[_]](elements: Vector[F[Expression[F]]]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Tuple[G] =
    Tuple(elements.map(e => phi(F.map(e)(_.mapK(phi)))))
}

case class Conditional[F[_]](condition: F[Expression[F]],
                             thenBranch: F[Expression[F]],
                             elseBranch: F[Expression[F]]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Conditional[G] =
    Conditional(
      phi(F.map(condition)(_.mapK(phi))),
      phi(F.map(thenBranch)(_.mapK(phi))),
      phi(F.map(elseBranch)(_.mapK(phi)))
    )
}

sealed trait Call[F[_]] extends Expression[F] {
  def target: F[Expression[F]]
}

object Call {
  def standard[F[_]](target: F[Expression[F]],
                     typeArgs: Vector[F[TypeArgument[F]]],
                     args: Vector[F[ArgumentGroup[F]]]): Call[F] =
    Standard(target, typeArgs, args)

  def infix[F[_]](left: F[Expression[F]],
                  operation: F[Identifier[F]],
                  typeArgs: Vector[F[TypeArgument[F]]],
                  right: F[Expression[F]]): Call[F] =
    Infix(left, operation, typeArgs, right)

  def postfix[F[_]](target: F[Expression[F]],
                    operation: F[Identifier[F]]): Call[F] =
    Postfix(target, operation)

  case class Standard[F[_]](target: F[Expression[F]],
                            typeArgs: Vector[F[TypeArgument[F]]],
                            args: Vector[F[ArgumentGroup[F]]]) extends Call[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Standard[G] =
      Standard(
        phi(F.map(target)(_.mapK(phi))),
        typeArgs.map(ta => phi(F.map(ta)(_.mapK(phi)))),
        args.map(ag => phi(F.map(ag)(_.mapK(phi))))
      )
  }

  case class Infix[F[_]](left: F[Expression[F]],
                         operation: F[Identifier[F]],
                         typeArgs: Vector[F[TypeArgument[F]]],
                         right: F[Expression[F]]) extends Call[F] {
    def target: F[Expression[F]] = left

    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Infix[G] =
      Infix(
        phi(F.map(left)(_.mapK(phi))),
        phi(F.map(operation)(_.mapK(phi))),
        typeArgs.map(ta => phi(F.map(ta)(_.mapK(phi)))),
        phi(F.map(right)(_.mapK(phi)))
      )
  }

  case class Postfix[F[_]](target: F[Expression[F]],
                           operation: F[Identifier[F]]) extends Call[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Postfix[G] =
      Postfix(
        phi(F.map(target)(_.mapK(phi))),
        phi(F.map(operation)(_.mapK(phi)))
      )
  }
}

case class Lambda[F[_]](params: Vector[F[LambdaParameter[F]]],
                        body: F[Expression[F]]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Lambda[G] =
    Lambda(
      params.map(p => phi(F.map(p)(_.mapK(phi)))),
      phi(F.map(body)(_.mapK(phi)))
    )
}

case class InterpolatedString[F[_]](interpolator: Interpolator,
                                    initial: String,
                                    segments: Vector[(F[Expression[F]], String)]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): InterpolatedString[G] =
    InterpolatedString(
      interpolator,
      initial,
      segments.map { case (e, s) => (phi(F.map(e)(_.mapK(phi))), s) }
    )
}

case class Match[F[_]](expression: F[Expression[F]],
                       cases: Vector[F[Case[F]]]) extends Expression[F] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Match[G] =
    Match(
      phi(F.map(expression)(_.mapK(phi))),
      cases.map(c => phi(F.map(c)(_.mapK(phi))))
    )
}

case class Case[F[_]](pattern: F[Pattern[F]],
                      guard: Option[F[Expression[F]]],
                      body: F[Expression[F]]) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Case[G] =
    Case(
      phi(F.map(pattern)(_.mapK(phi))),
      guard.map(g => phi(F.map(g)(_.mapK(phi)))),
      phi(F.map(body)(_.mapK(phi)))
    )
}
