package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ModuleSpec extends AnyFunSpec with Matchers {

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

      val mockRegistry = new ScalettaRegistry {
        def methodRegistry: MethodRegistry = ???

        def typeRegistry: TypeRegistry = ???

        def runtimeContextRegistry: RuntimeContextRegistry = ???
      }

      val result = module.register(mockRegistry)

      result shouldBe Seq(2, 6, 10, 14)
      registrationCount shouldBe 4
    }

    it("should handle an empty sequence") {
      val module = Module.traverse(Seq.empty[Int]) { item =>
        Module.pure(item)
      }

      val mockRegistry = new ScalettaRegistry {
        def methodRegistry: MethodRegistry = ???

        def typeRegistry: TypeRegistry = ???

        def runtimeContextRegistry: RuntimeContextRegistry = ???
      }

      val result = module.register(mockRegistry)
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

      val mockRegistry = new ScalettaRegistry {
        def methodRegistry: MethodRegistry = ???

        def typeRegistry: TypeRegistry = ???

        def runtimeContextRegistry: RuntimeContextRegistry = ???
      }

      val result = module.register(mockRegistry)

      result shouldBe Seq("A", "B", "C")
      order shouldBe Vector("a", "b", "c")
    }
  }

}
