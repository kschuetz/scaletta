package software.kes.scaletta.internal.library.standard

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.types.TypeRegistryImpl

final class StandardTypesSubtypingSpec extends AnyFunSpec with Matchers {
  describe("StandardTypes") {
    val registry = new TypeRegistryImpl
    val standardTypes = new StandardTypesImpl(registry)

    it("Some[A] should be a subtype of Option[A] (not an alias)") {
      val A = Type.variable(0)
      val someA = TypeApplier.fromNode(standardTypes.SomeT).applyAll(A)
      val optionA = TypeApplier.fromNode(standardTypes.OptionT).applyAll(A)

      val universe = registry.build()

      universe.hierarchy.isSubtype(someA, optionA) shouldBe true
      universe.hierarchy.isSubtype(optionA, someA) shouldBe false // This will fail if it's an alias
    }

    it("::[A] should be a subtype of List[A] (not an alias)") {
      val A = Type.variable(0)
      val consA = TypeApplier.fromNode(standardTypes.ConsT).applyAll(A)
      val listA = TypeApplier.fromNode(standardTypes.ListT).applyAll(A)

      val universe = registry.build()

      universe.hierarchy.isSubtype(consA, listA) shouldBe true
      universe.hierarchy.isSubtype(listA, consA) shouldBe false // This will fail if it's an alias
    }

    it("None should be a subtype of Option[Int] and share it with Some[Int]") {
      val optionInt = TypeApplier.fromNode(standardTypes.OptionT).applyAll(standardTypes.IntT)
      val optionString = TypeApplier.fromNode(standardTypes.OptionT).applyAll(standardTypes.StringT)
      val someInt = TypeApplier.fromNode(standardTypes.SomeT).applyAll(standardTypes.IntT)
      val hierarchy = registry.build().hierarchy

      hierarchy.isSubtype(standardTypes.NoneT, optionInt) shouldBe true
      hierarchy.isSubtype(standardTypes.NoneT, optionString) shouldBe true
      hierarchy.isSubtype(someInt, optionInt) shouldBe true
      hierarchy.isSubtype(optionInt, standardTypes.NoneT) shouldBe false
      hierarchy.relationshipFor(standardTypes.NoneT, someInt).commonSupertype shouldBe Some(optionInt)
      hierarchy.relationshipFor(someInt, standardTypes.NoneT).commonSupertype shouldBe Some(optionInt)
    }

    it("Nil should be a subtype of List[Int] and share it with ::[Int]") {
      val listInt = TypeApplier.fromNode(standardTypes.ListT).applyAll(standardTypes.IntT)
      val listString = TypeApplier.fromNode(standardTypes.ListT).applyAll(standardTypes.StringT)
      val consInt = TypeApplier.fromNode(standardTypes.ConsT).applyAll(standardTypes.IntT)
      val hierarchy = registry.build().hierarchy

      hierarchy.isSubtype(standardTypes.NilT, listInt) shouldBe true
      hierarchy.isSubtype(standardTypes.NilT, listString) shouldBe true
      hierarchy.isSubtype(consInt, listInt) shouldBe true
      hierarchy.isSubtype(listInt, standardTypes.NilT) shouldBe false
      hierarchy.relationshipFor(standardTypes.NilT, consInt).commonSupertype shouldBe Some(listInt)
      hierarchy.relationshipFor(consInt, standardTypes.NilT).commonSupertype shouldBe Some(listInt)
    }
  }
}
