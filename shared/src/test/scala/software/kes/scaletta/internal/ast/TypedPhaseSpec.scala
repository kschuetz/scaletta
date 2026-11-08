package software.kes.scaletta.internal.ast

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{NativeFunctionId, ProperType, Type, TypeId}
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.semantics.{LocalBindingKind, SemanticError}
import software.kes.scaletta.util.functional.~>

final class TypedPhaseSpec extends AnyFunSpec with Matchers {

  private val intType: ProperType[TypeId] = Type.nominal(TypeId(1))
  private val stringType: ProperType[TypeId] = Type.nominal(TypeId(2))

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
  }

  describe("TypedPhase.Expression") {
    it("should support mapK from Pos to Attributed") {
      val posToAttributed = new(Pos ~> Attributed) {
        def apply[A](fa: Pos[A]): Attributed[A] =
          Attributed(fa.value, fa.begin, fa.end, intType)
      }

      val identInfo: TypedInfo = TypedInfo.Local(0, 0, "n", LocalBindingKind.Val, intType)
      val pos = Pos[TypedInfo](identInfo, CharIndex(11), CharIndex(13))
      val ref = TypedPhase.Reference[Pos](pos)
      val transformed = ref.mapK(posToAttributed)

      val attr = Attributed[TypedInfo](identInfo, CharIndex(11), CharIndex(13), intType)

      transformed shouldBe TypedPhase.Reference[Attributed](attr)
    }

    it("should support mapK on Block and Typed with ascribed types") {
      val posToAttributed = new(Pos ~> Attributed) {
        def apply[A](fa: Pos[A]): Attributed[A] =
          Attributed(fa.value, fa.begin, fa.end, intType)
      }

      val identInfo: TypedInfo = TypedInfo.Local(0, 0, "n", LocalBindingKind.Val, intType)
      val identPos = Pos[TypedInfo](identInfo, CharIndex(11), CharIndex(13))
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

      val transformed = block.mapK(posToAttributed)

      val expectedIdentAttr = Attributed[TypedInfo](identInfo, CharIndex(11), CharIndex(13), intType)
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

}
