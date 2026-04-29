package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.MockSetup

class ModuleSpec extends AnyFunSpec with Matchers {

  private val mockSetup = MockSetup.create()

  describe("Module.traverse") {
    it("should sequence registrations and return a collection of results") {
      val items = Seq(1, 3, 5, 7) // Using primes as requested
      var registrationCount = 0

      val module = Module.traverse(items) { item =>
        Module { _ =>
          registrationCount += 1
          item * 2
        }
      }

      val result = module.configure(mockSetup)

      result shouldBe Seq(2, 6, 10, 14)
      registrationCount shouldBe 4
    }

    it("should handle an empty sequence") {
      val module = Module.traverse(Seq.empty[Int]) { item =>
        Module.pure(item)
      }

      val result = module.configure(mockSetup)
      result shouldBe Seq.empty
    }

    it("should preserve order of registrations") {
      val items = Seq("a", "b", "c")
      var order = Vector.empty[String]

      val module = Module.traverse(items) { item =>
        Module { _ =>
          order = order :+ item
          item.toUpperCase
        }
      }

      val result = module.configure(mockSetup)

      result shouldBe Seq("A", "B", "C")
      order shouldBe Vector("a", "b", "c")
    }
  }

  describe("Module.sequence") {
    it("should sequence registrations and return a collection of results") {
      val modules = Seq(Module.pure(41), Module.pure(43), Module.pure(47))
      val result = Module.sequence(modules).configure(mockSetup)
      result shouldBe Seq(41, 43, 47)
    }

    it("should handle an empty sequence") {
      val result = Module.sequence(Seq.empty[Module[Int]]).configure(mockSetup)
      result shouldBe Seq.empty
    }
  }

  describe("Module.when") {
    it("should execute the module when the condition is true") {
      var executed = false
      val module = Module { _ => executed = true }
      Module.when(condition = true)(module).configure(mockSetup)
      executed shouldBe true
    }

    it("should not execute the module when the condition is false") {
      var executed = false
      val module = Module { _ => executed = true }
      Module.when(condition = false)(module).configure(mockSetup)
      executed shouldBe false
    }
  }

  describe("Module.unless") {
    it("should execute the module when the condition is false") {
      var executed = false
      val module = Module { _ => executed = true }
      Module.unless(condition = false)(module).configure(mockSetup)
      executed shouldBe true
    }

    it("should not execute the module when the condition is true") {
      var executed = false
      val module = Module { _ => executed = true }
      Module.unless(condition = true)(module).configure(mockSetup)
      executed shouldBe false
    }
  }

  describe("Module.flatten") {
    it("should collapse nested modules") {
      val nested = Module.pure(Module.pure(41))
      val flattened = nested.flatten
      flattened.configure(mockSetup) shouldBe 41
    }

    it("should execute registrations in both layers") {
      var count1 = 0
      var count2 = 0
      val nested = Module { _ =>
        count1 += 1
        Module { _ =>
          count2 += 1
          43
        }
      }
      val flattened = nested.flatten
      val result = flattened.configure(mockSetup)
      result shouldBe 43
      count1 shouldBe 1
      count2 shouldBe 1
    }
  }

  describe("Module.tap") {
    it("should perform side-effects without changing the result") {
      var sideEffect = 0
      val module = Module.pure(43).tap(a => sideEffect = a)
      val result = module.configure(mockSetup)
      result shouldBe 43
      sideEffect shouldBe 43
    }
  }

}
