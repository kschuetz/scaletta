package software.kes.scaletta.internal.ast

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{NativeFunctionId, ProperType, Type, TypeId}
import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.internal.parser.ParseError
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.semantics.{LocalBindingKind, SemanticError}

final class TypedPhaseSpec extends AnyFunSpec with Matchers {

  private val intType: ProperType[TypeId] = Type.nominal(TypeId(1))
  private val stringType: ProperType[TypeId] = Type.nominal(TypeId(2))
  private val boolType: ProperType[TypeId] = Type.nominal(TypeId(3))
  private val unitType: ProperType[TypeId] = Type.unit

  describe("TypedInfo") {
    it("should provide properType for Local binding") {
      val info = TypedInfo.Local(depth = 0, slot = 1, name = "x", kind = LocalBindingKind.Val, tpe = intType)
      info.properType shouldBe Some(intType)
    }

    it("should provide properType for Bound symbol") {
      val symbolId = NativeFunctionSymbolId(NativeFunctionId(11))
      val info = TypedInfo.Bound(symbolId, intType)
      info.properType shouldBe Some(intType)
    }

    it("should provide properType for FieldAccess") {
      val info = TypedInfo.FieldAccess("length", intType)
      info.properType shouldBe Some(intType)
    }

    it("should return None for Error") {
      val info = TypedInfo.Error(SemanticError.UnknownIdentifier("x"))
      info.properType shouldBe None
    }
  }

  describe("Attributed") {
    it("should support functor mapping and position preservation") {
      val attr = Attributed("test", CharIndex(17), CharIndex(41), stringType)
      val mapped = attr.map(_.length)

      mapped.value shouldBe 4
      mapped.begin shouldBe CharIndex(17)
      mapped.end shouldBe CharIndex(41)
      mapped.properType shouldBe stringType
      mapped.pos shouldBe Pos(4, CharIndex(17), CharIndex(41))
    }

    it("should support withType") {
      val attr = Attributed(19, CharIndex(3), CharIndex(7), intType)
      val updated = attr.withType(stringType)

      updated.properType shouldBe stringType
      updated.value shouldBe 19
    }

    it("should create Attributed from Pos via fromPos") {
      val pos = Pos("hello", CharIndex(5), CharIndex(11))
      val attr = Attributed.fromPos(pos, stringType)

      attr.value shouldBe "hello"
      attr.begin shouldBe CharIndex(5)
      attr.end shouldBe CharIndex(11)
      attr.properType shouldBe stringType
    }

    it("should lift Pos to Attributed via Attributed.lift and TypedPhase.posToAttributed") {
      val nt1 = Attributed.lift(intType)
      val nt2 = TypedPhase.posToAttributed(intType)
      val pos = Pos("val", CharIndex(13), CharIndex(17))

      nt1(pos) shouldBe Attributed("val", CharIndex(13), CharIndex(17), intType)
      nt2(pos) shouldBe Attributed("val", CharIndex(13), CharIndex(17), intType)
    }

    it("should lift Pos to Attributed dynamically via Attributed.liftWith") {
      val nt = Attributed.liftWith {
        case Pos(info: TypedInfo, _, _) =>
          info.properType.getOrElse(unitType)
        case _ =>
          unitType
      }

      val info = TypedInfo.Local(0, 0, "s", LocalBindingKind.Val, stringType)
      val pos = Pos[TypedInfo](info, CharIndex(23), CharIndex(29))
      val attr = nt(pos)

      attr.properType shouldBe stringType
      attr.value shouldBe info
    }
  }

  describe("TypedPhase.Expression mapK") {
    val posToAttr = TypedPhase.posToAttributed(intType)

    val identInfo: TypedInfo = TypedInfo.Local(0, 0, "n", LocalBindingKind.Val, intType)
    val identPos = Pos[TypedInfo](identInfo, CharIndex(11), CharIndex(13))
    val expectedIdentAttr = Attributed[TypedInfo](identInfo, CharIndex(11), CharIndex(13), intType)

    it("should map Reference") {
      val ref = TypedPhase.Reference[Pos](identPos)
      ref.mapK(posToAttr) shouldBe TypedPhase.Reference[Attributed](expectedIdentAttr)
    }

    it("should map Select") {
      val qualifier = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(11), CharIndex(13))
      val fieldInfo: TypedInfo = TypedInfo.FieldAccess("size", intType)
      val fieldPos = Pos[TypedInfo](fieldInfo, CharIndex(14), CharIndex(18))

      val select = TypedPhase.Select[Pos](qualifier, fieldPos)
      val transformed = select.mapK(posToAttr)

      val expectedQualifier = Attributed[TypedPhase.Expression[Attributed]](
        TypedPhase.Reference[Attributed](expectedIdentAttr),
        CharIndex(11),
        CharIndex(13),
        intType
      )
      val expectedField = Attributed[TypedInfo](fieldInfo, CharIndex(14), CharIndex(18), intType)

      transformed shouldBe TypedPhase.Select[Attributed](expectedQualifier, expectedField)
    }

    it("should map Tuple and Conditional") {
      val expr1 = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(1), CharIndex(3))
      val expr2 = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(5), CharIndex(7))
      val condExpr = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(9), CharIndex(11))

      val tuple = TypedPhase.Tuple[Pos](Vector(expr1, expr2))
      val cond = TypedPhase.Conditional[Pos](condExpr, expr1, expr2)

      val expectedAttr1 = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(1), CharIndex(3), intType)
      val expectedAttr2 = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(5), CharIndex(7), intType)
      val expectedCondAttr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(9), CharIndex(11), intType)

      tuple.mapK(posToAttr) shouldBe TypedPhase.Tuple[Attributed](Vector(expectedAttr1, expectedAttr2))
      cond.mapK(posToAttr) shouldBe TypedPhase.Conditional[Attributed](expectedCondAttr, expectedAttr1, expectedAttr2)
    }

    it("should map Call.Standard and Call.Infix") {
      val target = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(1), CharIndex(5))
      val argExpr = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(7), CharIndex(9))
      val arg = Pos[TypedPhase.Argument[Pos]](TypedPhase.Argument[Pos](argExpr, Some(identPos)), CharIndex(7), CharIndex(9))
      val splatArg = Pos[TypedPhase.Argument[Pos]](TypedPhase.Argument[Pos](argExpr, None), CharIndex(11), CharIndex(13))
      val argGroup = Pos[TypedPhase.ArgumentGroup[Pos]](TypedPhase.ArgumentGroup[Pos](Vector(arg), Some(splatArg)), CharIndex(7), CharIndex(13))

      val typeIdentPos = Pos[TypedPhase.TypeIdent[Pos]](Right(intType), CharIndex(15), CharIndex(17))
      val typeArg = Pos[TypedPhase.TypeArgument[Pos]](TypedPhase.TypeArgument[Pos](typeIdentPos), CharIndex(15), CharIndex(17))

      val callStd = TypedPhase.Call.standard[Pos](target, Vector(typeArg), Vector(argGroup))
      val callInfix = TypedPhase.Call.infix[Pos](target, identPos, Vector(typeArg), argExpr)

      val expectedTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(1), CharIndex(5), intType)
      val expectedArgExpr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(7), CharIndex(9), intType)
      val expectedArg = Attributed[TypedPhase.Argument[Attributed]](TypedPhase.Argument[Attributed](expectedArgExpr, Some(expectedIdentAttr)), CharIndex(7), CharIndex(9), intType)
      val expectedSplatArg = Attributed[TypedPhase.Argument[Attributed]](TypedPhase.Argument[Attributed](expectedArgExpr, None), CharIndex(11), CharIndex(13), intType)
      val expectedArgGroup = Attributed[TypedPhase.ArgumentGroup[Attributed]](TypedPhase.ArgumentGroup[Attributed](Vector(expectedArg), Some(expectedSplatArg)), CharIndex(7), CharIndex(13), intType)
      val expectedTypeIdent = Attributed[TypedPhase.TypeIdent[Attributed]](Right(intType), CharIndex(15), CharIndex(17), intType)
      val expectedTypeArg = Attributed[TypedPhase.TypeArgument[Attributed]](TypedPhase.TypeArgument[Attributed](expectedTypeIdent), CharIndex(15), CharIndex(17), intType)

      callStd.mapK(posToAttr) shouldBe TypedPhase.Call.Standard[Attributed](expectedTarget, Vector(expectedTypeArg), Vector(expectedArgGroup))
      callInfix.mapK(posToAttr) shouldBe TypedPhase.Call.Infix[Attributed](expectedTarget, expectedIdentAttr, Vector(expectedTypeArg), expectedArgExpr)
    }

    it("should map Lambda and InterpolatedString") {
      val param = Pos[TypedPhase.LambdaParameter[Pos]](
        TypedPhase.LambdaParameter[Pos](identPos, Some(Pos(Right(intType), CharIndex(15), CharIndex(17)))),
        CharIndex(11),
        CharIndex(17)
      )
      val body = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(19), CharIndex(23))
      val lambda = TypedPhase.Lambda[Pos](Vector(param), body)

      val expectedParam = Attributed[TypedPhase.LambdaParameter[Attributed]](
        TypedPhase.LambdaParameter[Attributed](expectedIdentAttr, Some(Attributed(Right(intType), CharIndex(15), CharIndex(17), intType))),
        CharIndex(11),
        CharIndex(17),
        intType
      )
      val expectedBody = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(19), CharIndex(23), intType)

      lambda.mapK(posToAttr) shouldBe TypedPhase.Lambda[Attributed](Vector(expectedParam), expectedBody)

      val interp = TypedPhase.InterpolatedString[Pos](
        Interpolator.Custom("s"),
        "prefix ",
        Vector((body, " suffix"))
      )
      interp.mapK(posToAttr) shouldBe TypedPhase.InterpolatedString[Attributed](
        Interpolator.Custom("s"),
        "prefix ",
        Vector((expectedBody, " suffix"))
      )
    }

    it("should map Match and Case") {
      val expr = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(1), CharIndex(3))
      val pattern = Pos[TypedPhase.Pattern[Pos]](TypedPhase.Pattern.Identifier[Pos](identPos), CharIndex(5), CharIndex(7))
      val guard = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(9), CharIndex(11))
      val body = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(13), CharIndex(17))

      val caseClause = Pos[TypedPhase.Case[Pos]](TypedPhase.Case[Pos](pattern, Some(guard), body), CharIndex(5), CharIndex(17))
      val matchExpr = TypedPhase.Match[Pos](expr, Vector(caseClause))

      val expectedExpr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(1), CharIndex(3), intType)
      val expectedPattern = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Identifier[Attributed](expectedIdentAttr), CharIndex(5), CharIndex(7), intType)
      val expectedGuard = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(9), CharIndex(11), intType)
      val expectedBody = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(13), CharIndex(17), intType)
      val expectedCase = Attributed[TypedPhase.Case[Attributed]](TypedPhase.Case[Attributed](expectedPattern, Some(expectedGuard), expectedBody), CharIndex(5), CharIndex(17), intType)

      matchExpr.mapK(posToAttr) shouldBe TypedPhase.Match[Attributed](expectedExpr, Vector(expectedCase))
    }

    it("should map all Literal variants and Expression.Error") {
      TypedPhase.Literal.int[Pos](43).mapK(posToAttr) shouldBe TypedPhase.Literal.int[Attributed](43)
      TypedPhase.Literal.long[Pos](47L).mapK(posToAttr) shouldBe TypedPhase.Literal.long[Attributed](47L)
      TypedPhase.Literal.float[Pos](3.14f).mapK(posToAttr) shouldBe TypedPhase.Literal.float[Attributed](3.14f)
      TypedPhase.Literal.double[Pos](2.718).mapK(posToAttr) shouldBe TypedPhase.Literal.double[Attributed](2.718)
      TypedPhase.Literal.true_[Pos]().mapK(posToAttr) shouldBe TypedPhase.Literal.true_[Attributed]()
      TypedPhase.Literal.false_[Pos]().mapK(posToAttr) shouldBe TypedPhase.Literal.false_[Attributed]()
      TypedPhase.Literal.null_[Pos]().mapK(posToAttr) shouldBe TypedPhase.Literal.null_[Attributed]()
      TypedPhase.Literal.char[Pos]('k').mapK(posToAttr) shouldBe TypedPhase.Literal.char[Attributed]('k')
      TypedPhase.Literal.string[Pos]("scaletta").mapK(posToAttr) shouldBe TypedPhase.Literal.string[Attributed]("scaletta")
      TypedPhase.Literal.unit[Pos]().mapK(posToAttr) shouldBe TypedPhase.Literal.unit[Attributed]()

      val parseErr = ParseError.Message("expr error")
      TypedPhase.Expression.Error[Pos](parseErr).mapK(posToAttr) shouldBe TypedPhase.Expression.Error[Attributed](parseErr)
    }

    it("should support mapK on Block and Typed with ascribed types") {
      val typePos = Pos[TypedPhase.TypeIdent[Pos]](Right(intType), CharIndex(17), CharIndex(23))
      val exprPos = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(11), CharIndex(13))
      val typedExpr = TypedPhase.Typed[Pos](exprPos, typePos)

      val patternPos = Pos[TypedPhase.Pattern[Pos]](TypedPhase.Pattern.Identifier[Pos](identPos), CharIndex(5), CharIndex(7))
      val valDecl = Pos[TypedPhase.Declaration[Pos]](
        TypedPhase.Declaration.Val[Pos](patternPos, Pos(typedExpr, CharIndex(11), CharIndex(23))),
        CharIndex(1),
        CharIndex(23)
      )

      val block = TypedPhase.Block[Pos](
        Vector(valDecl),
        Pos(TypedPhase.Reference[Pos](identPos), CharIndex(29), CharIndex(31))
      )

      val transformed = block.mapK(posToAttr)

      val expectedTypeAttr = Attributed[TypedPhase.TypeIdent[Attributed]](Right(intType), CharIndex(17), CharIndex(23), intType)
      val expectedExprAttr = Attributed[TypedPhase.Expression[Attributed]](
        TypedPhase.Reference[Attributed](expectedIdentAttr),
        CharIndex(11),
        CharIndex(13),
        intType
      )
      val expectedTypedExpr = TypedPhase.Typed[Attributed](expectedExprAttr, expectedTypeAttr)
      val expectedPatternAttr = Attributed[TypedPhase.Pattern[Attributed]](
        TypedPhase.Pattern.Identifier[Attributed](expectedIdentAttr),
        CharIndex(5),
        CharIndex(7),
        intType
      )
      val expectedValDecl = Attributed[TypedPhase.Declaration[Attributed]](
        TypedPhase.Declaration.Val[Attributed](expectedPatternAttr, Attributed(expectedTypedExpr, CharIndex(11), CharIndex(23), intType)),
        CharIndex(1),
        CharIndex(23),
        intType
      )
      val expectedBlock = TypedPhase.Block[Attributed](
        Vector(expectedValDecl),
        Attributed(TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(29), CharIndex(31), intType)
      )

      transformed shouldBe expectedBlock
    }
  }

  describe("TypedPhase.Declaration mapK") {
    val posToAttr = TypedPhase.posToAttributed(intType)

    val identInfo: TypedInfo = TypedInfo.Local(0, 0, "x", LocalBindingKind.Val, intType)
    val identPos = Pos[TypedInfo](identInfo, CharIndex(5), CharIndex(7))
    val expectedIdentAttr = Attributed[TypedInfo](identInfo, CharIndex(5), CharIndex(7), intType)

    val patternPos = Pos[TypedPhase.Pattern[Pos]](TypedPhase.Pattern.Identifier[Pos](identPos), CharIndex(5), CharIndex(7))
    val expectedPatternAttr = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Identifier[Attributed](expectedIdentAttr), CharIndex(5), CharIndex(7), intType)

    val rhsPos = Pos[TypedPhase.Expression[Pos]](TypedPhase.Reference[Pos](identPos), CharIndex(11), CharIndex(13))
    val expectedRhsAttr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference[Attributed](expectedIdentAttr), CharIndex(11), CharIndex(13), intType)

    it("should map Val and LazyVal") {
      val valDecl = TypedPhase.Declaration.val_[Pos](patternPos, rhsPos)
      val lazyValDecl = TypedPhase.Declaration.lazyVal[Pos](patternPos, rhsPos)

      valDecl.mapK(posToAttr) shouldBe TypedPhase.Declaration.Val[Attributed](expectedPatternAttr, expectedRhsAttr)
      lazyValDecl.mapK(posToAttr) shouldBe TypedPhase.Declaration.LazyVal[Attributed](expectedPatternAttr, expectedRhsAttr)
    }

    it("should map Def with FormalParameterGroup") {
      val typePos = Pos[TypedPhase.TypeIdent[Pos]](Right(intType), CharIndex(15), CharIndex(19))
      val expectedTypeAttr = Attributed[TypedPhase.TypeIdent[Attributed]](Right(intType), CharIndex(15), CharIndex(19), intType)

      val param = Pos[TypedPhase.FormalParameter[Pos]](
        TypedPhase.FormalParameter[Pos](identPos, typePos, Some(rhsPos)),
        CharIndex(5),
        CharIndex(19)
      )
      val expectedParamAttr = Attributed[TypedPhase.FormalParameter[Attributed]](
        TypedPhase.FormalParameter[Attributed](expectedIdentAttr, expectedTypeAttr, Some(expectedRhsAttr)),
        CharIndex(5),
        CharIndex(19),
        intType
      )

      val paramGroup = Pos[TypedPhase.FormalParameterGroup[Pos]](
        TypedPhase.FormalParameterGroup[Pos](Vector(param), None),
        CharIndex(5),
        CharIndex(19)
      )
      val expectedParamGroupAttr = Attributed[TypedPhase.FormalParameterGroup[Attributed]](
        TypedPhase.FormalParameterGroup[Attributed](Vector(expectedParamAttr), None),
        CharIndex(5),
        CharIndex(19),
        intType
      )

      val defDecl = TypedPhase.Declaration.def_[Pos](identPos, Vector(paramGroup), Some(typePos), rhsPos)

      defDecl.mapK(posToAttr) shouldBe TypedPhase.Declaration.Def[Attributed](
        expectedIdentAttr,
        Vector(expectedParamGroupAttr),
        Some(expectedTypeAttr),
        expectedRhsAttr
      )
    }

    it("should map Declaration.Error") {
      val parseErr = ParseError.Message("decl error")
      TypedPhase.Declaration.Error[Pos](parseErr).mapK(posToAttr) shouldBe TypedPhase.Declaration.Error[Attributed](parseErr)
    }
  }

  describe("TypedPhase.Pattern mapK") {
    val posToAttr = TypedPhase.posToAttributed(intType)

    val identInfo: TypedInfo = TypedInfo.Local(0, 0, "x", LocalBindingKind.Val, intType)
    val identPos = Pos[TypedInfo](identInfo, CharIndex(3), CharIndex(5))
    val expectedIdentAttr = Attributed[TypedInfo](identInfo, CharIndex(3), CharIndex(5), intType)

    it("should map Pattern.Identifier and Pattern.Wildcard") {
      TypedPhase.Pattern.Identifier[Pos](identPos).mapK(posToAttr) shouldBe TypedPhase.Pattern.Identifier[Attributed](expectedIdentAttr)
      TypedPhase.Pattern.Wildcard[Pos]().mapK(posToAttr) shouldBe TypedPhase.Pattern.Wildcard[Attributed]()
    }

    it("should map Pattern.Literal, Pattern.As, Pattern.Typed, Pattern.Tuple, Pattern.Product") {
      val litPos = Pos[TypedPhase.Literal[Pos]](TypedPhase.Literal.int[Pos](71), CharIndex(7), CharIndex(9))
      val patLit = TypedPhase.Pattern.Literal[Pos](litPos)
      val expectedLitAttr = Attributed[TypedPhase.Literal[Attributed]](TypedPhase.Literal.int[Attributed](71), CharIndex(7), CharIndex(9), intType)

      patLit.mapK(posToAttr) shouldBe TypedPhase.Pattern.Literal[Attributed](expectedLitAttr)

      val subPatternPos = Pos[TypedPhase.Pattern[Pos]](TypedPhase.Pattern.Identifier[Pos](identPos), CharIndex(3), CharIndex(5))
      val expectedSubPatternAttr = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Identifier[Attributed](expectedIdentAttr), CharIndex(3), CharIndex(5), intType)

      val asPat = TypedPhase.Pattern.As[Pos](identPos, subPatternPos)
      asPat.mapK(posToAttr) shouldBe TypedPhase.Pattern.As[Attributed](expectedIdentAttr, expectedSubPatternAttr)

      val typePos = Pos[TypedPhase.TypeIdent[Pos]](Right(intType), CharIndex(11), CharIndex(13))
      val expectedTypeAttr = Attributed[TypedPhase.TypeIdent[Attributed]](Right(intType), CharIndex(11), CharIndex(13), intType)

      val typedPat = TypedPhase.Pattern.Typed[Pos](subPatternPos, typePos)
      typedPat.mapK(posToAttr) shouldBe TypedPhase.Pattern.Typed[Attributed](expectedSubPatternAttr, expectedTypeAttr)

      val tuplePat = TypedPhase.Pattern.Tuple[Pos](Vector(subPatternPos))
      tuplePat.mapK(posToAttr) shouldBe TypedPhase.Pattern.Tuple[Attributed](Vector(expectedSubPatternAttr))

      val productPat = TypedPhase.Pattern.Product[Pos](typePos, Vector(subPatternPos))
      productPat.mapK(posToAttr) shouldBe TypedPhase.Pattern.Product[Attributed](expectedTypeAttr, Vector(expectedSubPatternAttr))
    }

    it("should map Pattern.Error") {
      val parseErr = ParseError.Message("pattern error")
      TypedPhase.Pattern.Error[Pos](parseErr).mapK(posToAttr) shouldBe TypedPhase.Pattern.Error[Attributed](parseErr)
    }
  }

}
