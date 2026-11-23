package software.kes.scaletta.internal.interpolators

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes

final class InterpolatorRegistrySpec extends AnyFunSpec with Matchers {

  private def dummyInterpolator(returnType: ProperType[TypeId] = CoreTypes.StringT): CustomInterpolator =
    CustomInterpolator(
      returnType = returnType,
      impl = FunctionImpl.objectResult(_ => "test")
    )

  describe("InterpolatorRegistryImpl") {
    it("should register and look up interpolators by Name and InterpolatorId") {
      val registry = InterpolatorRegistryImpl.create()
      val interp1 = dummyInterpolator(CoreTypes.StringT)
      val interp2 = dummyInterpolator(CoreTypes.IntT)

      registry.registerInterpolator(Name("s"), interp1)
      registry.registerInterpolator(Name("custom"), interp2)

      val id1 = registry.getInterpolatorByName(Name("s"))
      val id2 = registry.getInterpolatorByName(Name("custom"))

      id1 shouldBe Some(InterpolatorId(0))
      id2 shouldBe Some(InterpolatorId(1))

      registry.getCustomInterpolator(InterpolatorId(0)) shouldBe interp1
      registry.getCustomInterpolator(InterpolatorId(1)) shouldBe interp2
    }

    it("should return None for unregistered interpolator names") {
      val registry = InterpolatorRegistryImpl.create()
      registry.getInterpolatorByName(Name("unknown")) shouldBe None
    }

    it("should throw NoSuchElementException for unregistered InterpolatorId") {
      val registry = InterpolatorRegistryImpl.create()
      a[NoSuchElementException] should be thrownBy {
        registry.getCustomInterpolator(InterpolatorId(43))
      }
    }

    it("should allow updating definition for existing name without changing its id") {
      val registry = InterpolatorRegistryImpl.create()
      val interpOld = dummyInterpolator(CoreTypes.StringT)
      val interpNew = dummyInterpolator(CoreTypes.IntT)

      registry.registerInterpolator(Name("json"), interpOld)
      registry.getInterpolatorByName(Name("json")) shouldBe Some(InterpolatorId(0))
      registry.getCustomInterpolator(InterpolatorId(0)) shouldBe interpOld

      registry.registerInterpolator(Name("json"), interpNew)
      registry.getInterpolatorByName(Name("json")) shouldBe Some(InterpolatorId(0))
      registry.getCustomInterpolator(InterpolatorId(0)) shouldBe interpNew
    }

    it("should build an immutable InterpolatorLookup with matching lookups") {
      val registry = InterpolatorRegistryImpl.create()
      val interp = dummyInterpolator(CoreTypes.StringT)
      registry.registerInterpolator(Name("s"), interp)

      val lookup = registry.build()

      lookup.getInterpolatorByName(Name("s")) shouldBe Some(InterpolatorId(0))
      lookup.getInterpolatorByName(Name("unknown")) shouldBe None
      lookup.getCustomInterpolator(InterpolatorId(0)) shouldBe interp

      a[NoSuchElementException] should be thrownBy {
        lookup.getCustomInterpolator(InterpolatorId(41))
      }
    }
  }

  describe("InterpolatorId") {
    it("should create InterpolatorId instances via apply factory") {
      val id = InterpolatorId(41)
      id.value shouldBe 41
      id.toString shouldBe "InterpolatorId(41)"
    }
  }
}
