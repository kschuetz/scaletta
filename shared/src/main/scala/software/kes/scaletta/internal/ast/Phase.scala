package software.kes.scaletta.internal.ast

import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.internal.parser.ParseError
import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

trait Phase {
  phase =>
  type Ident[_[_]]

  type TypeIdent[_[_]]

  protected implicit def identFunctorK: FunctorK[Ident]

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent]

  sealed trait Expression[F[_]] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Expression[G]
  }

  object Expression {
    case class Error[F[_]](error: ParseError) extends Expression[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Error[G] = Error(error)
    }
  }

  case class Block[F[_]](declarations: Vector[F[Declaration[F]]],
                         result: F[Expression[F]]) extends Expression[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Block[G] =
      Block(
        declarations.map(d => phi(F.map(d)(_.mapK(phi)))),
        phi(F.map(result)(_.mapK(phi)))
      )
  }

  case class Select[F[_]](qualifier: F[Expression[F]], name: F[Ident[F]]) extends Expression[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Select[G] =
      Select(phi(F.map(qualifier)(_.mapK(phi))), phi(F.map(name)(ident => identFunctorK.mapK(ident)(phi))))
  }

  case class Reference[F[_]](id: F[Ident[F]]) extends Expression[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Reference[G] =
      Reference(phi(F.map(id)(ident => identFunctorK.mapK(ident)(phi))))
  }

  case class Typed[F[_]](expression: F[Expression[F]],
                         ascription: F[TypeIdent[F]]) extends Expression[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Typed[G] =
      Typed(
        phi(F.map(expression)(_.mapK(phi))),
        phi(F.map(ascription)(typeIdent => typeIdentFunctorK.mapK(typeIdent)(phi)))
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

    def unit[F[_]](): Literal[F] = UnitLiteral()

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

    case class UnitLiteral[F[_]]() extends Literal[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): UnitLiteral[G] = UnitLiteral()
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
                    operation: F[Ident[F]],
                    typeArgs: Vector[F[TypeArgument[F]]],
                    right: F[Expression[F]]): Call[F] =
      Infix(left, operation, typeArgs, right)

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
                           operation: F[Ident[F]],
                           typeArgs: Vector[F[TypeArgument[F]]],
                           right: F[Expression[F]]) extends Call[F] {
      def target: F[Expression[F]] = left

      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Infix[G] =
        Infix(
          phi(F.map(left)(_.mapK(phi))),
          phi(F.map(operation)(ident => identFunctorK.mapK(ident)(phi))),
          typeArgs.map(ta => phi(F.map(ta)(_.mapK(phi)))),
          phi(F.map(right)(_.mapK(phi)))
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

  sealed trait Declaration[F[_]] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Declaration[G]
  }

  object Declaration {
    def val_[F[_]](pattern: F[Pattern[F]], rhs: F[Expression[F]]): Declaration[F] =
      Val(pattern, rhs)

    def lazyVal[F[_]](pattern: F[Pattern[F]], rhs: F[Expression[F]]): Declaration[F] =
      LazyVal(pattern, rhs)

    def def_[F[_]](name: F[Ident[F]],
                   params: Vector[F[FormalParameterGroup[F]]],
                   returnType: Option[F[TypeIdent[F]]],
                   body: F[Expression[F]]): Declaration[F] =
      Def(name, params, returnType, body)

    case class Val[F[_]](pattern: F[Pattern[F]],
                         rhs: F[Expression[F]]) extends Declaration[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Val[G] =
        Val(
          phi(F.map(pattern)(_.mapK(phi))),
          phi(F.map(rhs)(_.mapK(phi)))
        )
    }

    case class Def[F[_]](name: F[Ident[F]],
                         params: Vector[F[FormalParameterGroup[F]]],
                         returnType: Option[F[TypeIdent[F]]],
                         body: F[Expression[F]]) extends Declaration[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Def[G] =
        Def(
          phi(F.map(name)(ident => identFunctorK.mapK(ident)(phi))),
          params.map(p => phi(F.map(p)(_.mapK(phi)))),
          returnType.map(rt => phi(F.map(rt)(typeIdent => typeIdentFunctorK.mapK(typeIdent)(phi)))),
          phi(F.map(body)(_.mapK(phi)))
        )
    }

    case class LazyVal[F[_]](pattern: F[Pattern[F]],
                             rhs: F[Expression[F]]) extends Declaration[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): LazyVal[G] =
        LazyVal(
          phi(F.map(pattern)(_.mapK(phi))),
          phi(F.map(rhs)(_.mapK(phi)))
        )
    }

    case class Error[F[_]](error: ParseError) extends Declaration[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Error[G] = Error(error)
    }
  }

  case class FormalParameter[F[_]](name: F[Ident[F]],
                                   typ: F[TypeIdent[F]],
                                   default: Option[F[Expression[F]]] = None) {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): FormalParameter[G] =
      FormalParameter(
        phi(F.map(name)(ident => identFunctorK.mapK(ident)(phi))),
        phi(F.map(typ)(typeIdent => typeIdentFunctorK.mapK(typeIdent)(phi))),
        default.map(d => phi(F.map(d)(_.mapK(phi))))
      )
  }

  case class FormalParameterGroup[F[_]](parameters: Vector[F[FormalParameter[F]]],
                                        variadic: Option[F[FormalParameter[F]]] = None) {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): FormalParameterGroup[G] =
      FormalParameterGroup(
        parameters.map(p => phi(F.map(p)(_.mapK(phi)))),
        variadic.map(v => phi(F.map(v)(_.mapK(phi))))
      )
  }

  case class LambdaParameter[F[_]](name: F[Ident[F]], typ: Option[F[TypeIdent[F]]]) {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): LambdaParameter[G] =
      LambdaParameter(phi(F.map(name)(ident => identFunctorK.mapK(ident)(phi))),
        typ.map(t => phi(F.map(t)(typeIdent => typeIdentFunctorK.mapK(typeIdent)(phi)))))
  }

  case class Argument[F[_]](value: F[Expression[F]],
                            name: Option[F[Ident[F]]] = None) {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Argument[G] =
      Argument(phi(F.map(value)(_.mapK(phi))), name.map(n => phi(F.map(n)(ident => identFunctorK.mapK(ident)(phi)))))
  }

  case class ArgumentGroup[F[_]](arguments: Vector[F[Argument[F]]],
                                 splat: Option[F[Argument[F]]] = None) {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): ArgumentGroup[G] =
      ArgumentGroup(
        arguments.map(a => phi(F.map(a)(_.mapK(phi)))),
        splat.map(s => phi(F.map(s)(_.mapK(phi))))
      )
  }

  case class TypeArgument[F[_]](typ: F[TypeIdent[F]]) {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): TypeArgument[G] =
      TypeArgument(phi(F.map(typ)(typeIdent => typeIdentFunctorK.mapK(typeIdent)(phi))))
  }

  sealed trait Pattern[F[_]] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Pattern[G]
  }

  object Pattern {
    /** Matches any value and binds it to a name. */
    case class Identifier[F[_]](name: F[Ident[F]]) extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Identifier[G] =
        Identifier(phi(F.map(name)(ident => identFunctorK.mapK(ident)(phi))))
    }

    /** Wildcard pattern `_` that matches anything without binding. */
    case class Wildcard[F[_]]() extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Wildcard[G] = Wildcard()
    }

    /** Matches an exact literal value (Int, String, Null, etc.). */
    case class Literal[F[_]](value: F[phase.Literal[F]]) extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Literal[G] =
        Literal(phi(F.map(value)(_.mapK(phi).asInstanceOf[phase.Literal[G]])))
    }

    /** Binds a name to a value if it matches another pattern: `name @ pattern`. */
    case class As[F[_]](name: F[Ident[F]], pattern: F[Pattern[F]]) extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): As[G] =
        As(phi(F.map(name)(ident => identFunctorK.mapK(ident)(phi))), phi(F.map(pattern)(_.mapK(phi))))
    }

    /** Matches if the value satisfies a type test: `pattern: Type`. */
    case class Typed[F[_]](pattern: F[Pattern[F]], ascription: F[TypeIdent[F]]) extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Typed[G] =
        Typed(phi(F.map(pattern)(_.mapK(phi))), phi(F.map(ascription)(typeIdent => typeIdentFunctorK.mapK(typeIdent)(phi))))
    }

    /** Positional destructuring of tuples: `(p1, p2, ...)`. */
    case class Tuple[F[_]](elements: Vector[F[Pattern[F]]]) extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Tuple[G] =
        Tuple(elements.map(e => phi(F.map(e)(_.mapK(phi)))))
    }

    /**
     * Constructor-like pattern for host-provided product types: `Some(p1)`.
     * This is necessary to support matching on `Option` types.
     */
    case class Product[F[_]](typeId: F[TypeIdent[F]], args: Vector[F[Pattern[F]]]) extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Product[G] =
        Product(phi(F.map(typeId)(typeIdent => typeIdentFunctorK.mapK(typeIdent)(phi))),
          args.map(a => phi(F.map(a)(_.mapK(phi)))))
    }

    case class Error[F[_]](error: ParseError) extends Pattern[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Error[G] = Error(error)
    }
  }

}
