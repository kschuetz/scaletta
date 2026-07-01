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
  }
}
