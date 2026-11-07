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
      val innerPos = Pos[TypedInfo](identInfo, CharIndex(11), CharIndex(13))
      val outerPos = Pos[Pos[TypedInfo]](innerPos, CharIndex(11), CharIndex(13))
      val ref = TypedPhase.Reference[Pos](outerPos)
      val transformed = ref.mapK(posToAttributed)

      val innerAttr = Attributed[TypedInfo](identInfo, CharIndex(11), CharIndex(13), intType)
      val outerAttr = Attributed[Attributed[TypedInfo]](innerAttr, CharIndex(11), CharIndex(13), intType)

      transformed shouldBe TypedPhase.Reference[Attributed](outerAttr)
    }
  }

}
