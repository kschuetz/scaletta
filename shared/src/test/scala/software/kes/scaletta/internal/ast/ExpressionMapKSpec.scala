package software.kes.scaletta.internal.ast

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.internal.ast.ParsingPhase._
import software.kes.scaletta.util.functional.Id._
import software.kes.scaletta.util.functional.~>

class ExpressionMapKSpec extends AnyFunSpec with Matchers {

  private val idToOption = new(Id ~> Option) {
    def apply[A](fa: Id[A]): Option[A] = Some(fa)
  }

  describe("Expression.mapK") {

    it("should transform Block") {
      val expr = Block[Id](
        Vector(Declaration.val_[Id](Pattern.Wildcard[Id](), Literal.IntLiteral[Id](1))),
        Literal.IntLiteral[Id](2)
      )
      val result = expr.mapK(idToOption)
      result shouldBe Block[Option](
        Vector(Some(Declaration.Val[Option](Some(Pattern.Wildcard[Option]()), Some(Literal.IntLiteral[Option](1))))),
        Some(Literal.IntLiteral[Option](2))
      )
    }

    it("should transform Reference") {
      val expr = Reference[Id](Identifier[Id]("a"))
      val result = expr.mapK(idToOption)
      result shouldBe Reference[Option](Some(Identifier[Option]("a")))
    }

    it("should transform Select") {
      val expr = Select[Id](Reference[Id](Identifier[Id]("a")), Identifier[Id]("b"))
      val result = expr.mapK(idToOption)
      result shouldBe Select[Option](Some(Reference[Option](Some(Identifier[Option]("a")))), Some(Identifier[Option]("b")))
    }

    it("should transform Typed") {
      val expr = Typed[Id](Literal.Null[Id](), TypeIdentifier.name[Id](Identifier[Id]("Int")))
      val result = expr.mapK(idToOption)
      result shouldBe Typed[Option](Some(Literal.Null[Option]()), Some(TypeIdentifier.name[Option](Some(Identifier[Option]("Int")))))
    }

    it("should transform all Literal types") {
      Literal.IntLiteral[Id](1).mapK(idToOption) shouldBe Literal.IntLiteral[Option](1)
      Literal.LongLiteral[Id](1L).mapK(idToOption) shouldBe Literal.LongLiteral[Option](1)
      Literal.FloatLiteral[Id](1.0f).mapK(idToOption) shouldBe Literal.FloatLiteral[Option](1.0f)
      Literal.DoubleLiteral[Id](1.0).mapK(idToOption) shouldBe Literal.DoubleLiteral[Option](1.0)
      Literal.True[Id]().mapK(idToOption) shouldBe Literal.True[Option]()
      Literal.False[Id]().mapK(idToOption) shouldBe Literal.False[Option]()
      Literal.Null[Id]().mapK(idToOption) shouldBe Literal.Null[Option]()
      Literal.CharLiteral[Id]('a').mapK(idToOption) shouldBe Literal.CharLiteral[Option]('a')
      Literal.StringLiteral[Id]("hi").mapK(idToOption) shouldBe Literal.StringLiteral[Option]("hi")
    }

    it("should transform Tuple") {
      val expr = Tuple[Id](Vector(Literal.IntLiteral[Id](1), Literal.StringLiteral[Id]("a")))
      val result = expr.mapK(idToOption)
      result shouldBe Tuple[Option](Vector(Some(Literal.IntLiteral[Option](1)), Some(Literal.StringLiteral[Option]("a"))))
    }

    it("should transform Conditional") {
      val expr = Conditional[Id](Literal.True[Id](), Literal.IntLiteral[Id](1), Literal.IntLiteral[Id](2))
      val result = expr.mapK(idToOption)
      result shouldBe Conditional[Option](
        Some(Literal.True[Option]()),
        Some(Literal.IntLiteral[Option](1)),
        Some(Literal.IntLiteral[Option](2))
      )
    }

    it("should transform Call.Standard") {
      val target = Reference[Id](Identifier[Id]("f"))
      val typeArg = TypeArgument[Id](TypeIdentifier.name[Id](Identifier[Id]("T")))
      val argGroup = ArgumentGroup[Id](Vector(Argument[Id](Literal.IntLiteral[Id](1))))
      val expr = Call.Standard[Id](target, Vector(typeArg), Vector(argGroup))

      val result = expr.mapK(idToOption)
      result shouldBe Call.Standard[Option](
        Some(target.mapK(idToOption)),
        Vector(Some(typeArg.mapK(idToOption))),
        Vector(Some(argGroup.mapK(idToOption)))
      )
    }

    it("should transform Call.Infix") {
      val left = Literal.IntLiteral[Id](1)
      val right = Literal.IntLiteral[Id](2)
      val op = Identifier[Id]("+")
      val expr = Call.Infix[Id](left, op, Vector.empty, right)

      val result = expr.mapK(idToOption)
      result shouldBe Call.Infix[Option](
        Some(left.mapK(idToOption)),
        Some(op.mapK(idToOption)),
        Vector.empty,
        Some(right.mapK(idToOption))
      )
    }

    it("should transform Lambda") {
      val param = LambdaParameter[Id](Identifier[Id]("x"), Some(TypeIdentifier.name[Id](Identifier[Id]("Int"))))
      val body = Reference[Id](Identifier[Id]("x"))
      val expr = Lambda[Id](Vector(param), body)

      val result = expr.mapK(idToOption)
      result shouldBe Lambda[Option](
        Vector(Some(param.mapK(idToOption))),
        Some(body.mapK(idToOption))
      )
    }

    it("should transform InterpolatedString") {
      val expr = InterpolatedString[Id](
        Interpolator.Custom("s"),
        "pre",
        Vector((Literal.IntLiteral[Id](1), "post"))
      )
      val result = expr.mapK(idToOption)
      result shouldBe InterpolatedString[Option](
        Interpolator.Custom("s"),
        "pre",
        Vector((Some(Literal.IntLiteral[Option](1)), "post"))
      )
    }

    it("should transform Match and Case") {
      val pattern = Pattern.Wildcard[Id]()
      val guard = Some(Literal.True[Id]())
      val body = Literal.IntLiteral[Id](1)
      val kase = Case[Id](pattern, guard, body)
      val expr = Match[Id](Literal.Null[Id](), Vector(kase))

      val result = expr.mapK(idToOption)
      result shouldBe Match[Option](
        Some(Literal.Null[Option]()),
        Vector(Some(Case[Option](
          Some(pattern.mapK(idToOption)),
          Some(Some(Literal.True[Option]())),
          Some(body.mapK(idToOption))
        )))
      )
    }
  }

  describe("Declaration.mapK") {
    it("should transform Val") {
      val expr = Declaration.val_[Id](Pattern.Wildcard[Id](), Literal.IntLiteral[Id](1))
      val result = expr.mapK(idToOption)
      result shouldBe Declaration.Val[Option](Some(Pattern.Wildcard[Option]()), Some(Literal.IntLiteral[Option](1)))
    }

    it("should transform Def") {
      val paramGroup = FormalParameterGroup[Id](Vector(FormalParameter[Id](Identifier[Id]("p"), TypeIdentifier.name[Id](Identifier[Id]("Int")), None)))
      val expr = Declaration.def_[Id](Identifier[Id]("f"), Vector(paramGroup), None, Literal.Null[Id]())
      val result = expr.mapK(idToOption)
      result shouldBe Declaration.Def[Option](
        Some(Identifier[Option]("f")),
        Vector(Some(paramGroup.mapK(idToOption))),
        None,
        Some(Literal.Null[Option]())
      )
    }

    it("should transform LazyVal") {
      val expr = Declaration.lazyVal[Id](Pattern.Wildcard[Id](), Literal.IntLiteral[Id](1))
      val result = expr.mapK(idToOption)
      result shouldBe Declaration.LazyVal[Option](Some(Pattern.Wildcard[Option]()), Some(Literal.IntLiteral[Option](1)))
    }
  }

  describe("Pattern.mapK") {
    it("should transform Identifier pattern") {
      val pat = Pattern.Identifier[Id](Identifier[Id]("x"))
      val result = pat.mapK(idToOption)
      result shouldBe Pattern.Identifier[Option](Some(Identifier[Option]("x")))
    }

    it("should transform Wildcard pattern") {
      val pat = Pattern.Wildcard[Id]()
      val result = pat.mapK(idToOption)
      result shouldBe Pattern.Wildcard[Option]()
    }

    it("should transform Literal pattern") {
      val lit = Literal.IntLiteral[Id](1)
      val pat = Pattern.Literal[Id](lit)
      val result = pat.mapK(idToOption)
      result shouldBe Pattern.Literal[Option](Some(Literal.IntLiteral[Option](1)))
    }

    it("should transform As pattern") {
      val pat = Pattern.As[Id](Identifier[Id]("x"), Pattern.Wildcard[Id]())
      val result = pat.mapK(idToOption)
      result shouldBe Pattern.As[Option](Some(Identifier[Option]("x")), Some(Pattern.Wildcard[Option]()))
    }

    it("should transform Typed pattern") {
      val pat = Pattern.Typed[Id](Pattern.Wildcard[Id](), TypeIdentifier.name[Id](Identifier[Id]("Int")))
      val result = pat.mapK(idToOption)
      result shouldBe Pattern.Typed[Option](Some(Pattern.Wildcard[Option]()), Some(TypeIdentifier.name[Option](Some(Identifier[Option]("Int")))))
    }

    it("should transform Tuple pattern") {
      val pat = Pattern.Tuple[Id](Vector(Pattern.Wildcard[Id](), Pattern.Identifier[Id](Identifier[Id]("y"))))
      val result = pat.mapK(idToOption)
      result shouldBe Pattern.Tuple[Option](Vector(Some(Pattern.Wildcard[Option]()), Some(Pattern.Identifier[Option](Some(Identifier[Option]("y"))))))
    }

    it("should transform Product pattern") {
      val pat = Pattern.Product[Id](TypeIdentifier.name[Id](Identifier[Id]("Some")), Vector(Pattern.Wildcard[Id]()))
      val result = pat.mapK(idToOption)
      result shouldBe Pattern.Product[Option](Some(TypeIdentifier.name[Option](Some(Identifier[Option]("Some")))), Vector(Some(Pattern.Wildcard[Option]())))
    }
  }
}
