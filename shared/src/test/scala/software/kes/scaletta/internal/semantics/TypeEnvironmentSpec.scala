package software.kes.scaletta.internal.semantics

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{ProperType, Type, TypeId}

final class TypeEnvironmentSpec extends AnyFunSpec with Matchers {

  private val typeInt: ProperType[TypeId] = Type.nominal(TypeId(3))
  private val typeString: ProperType[TypeId] = Type.nominal(TypeId(5))
  private val typeBoolean: ProperType[TypeId] = Type.nominal(TypeId(7))
  private val typeDouble: ProperType[TypeId] = Type.nominal(TypeId(11))

  describe("TypeEnvironment") {

    it("should return None when looking up any identifier in empty environment") {
      val env = TypeEnvironment.empty
      env.lookup("x") shouldBe None
    }

    it("should resolve root-level bindings with exact slot, kind, and type") {
      val bindingX = TypedScopeBinding("x", slot = 0, LocalBindingKind.Val, typeInt)
      val bindingY = TypedScopeBinding("y", slot = 1, LocalBindingKind.LazyVal, typeString)
      val env = TypeEnvironment.empty.pushLayer(Vector(bindingX, bindingY))

      env.lookup("x") shouldBe Some((0, bindingX))
      env.lookup("y") shouldBe Some((0, bindingY))
      env.lookup("z") shouldBe None
    }

    it("should support sequential extension of the active layer with extendCurrentLayer") {
      val bindingA = TypedScopeBinding("a", slot = 0, LocalBindingKind.Val, typeInt)
      val bindingB = TypedScopeBinding("b", slot = 1, LocalBindingKind.Val, typeBoolean)

      val env0 = TypeEnvironment.empty.pushLayer(Vector.empty)
      val env1 = env0.extendCurrentLayer(bindingA)
      val env2 = env1.extendCurrentLayer(bindingB)

      env0.lookup("a") shouldBe None
      env1.lookup("a") shouldBe Some((0, bindingA))
      env1.lookup("b") shouldBe None

      env2.lookup("a") shouldBe Some((0, bindingA))
      env2.lookup("b") shouldBe Some((0, bindingB))
    }

    it("should extend empty root environment when extendCurrentLayer is called on empty") {
      val bindingA = TypedScopeBinding("a", slot = 0, LocalBindingKind.Val, typeInt)
      val env = TypeEnvironment.empty.extendCurrentLayer(bindingA)

      env.lookup("a") shouldBe Some((0, bindingA))
    }

    it("should correctly handle shadowing across nested layers") {
      val outerX = TypedScopeBinding("x", slot = 0, LocalBindingKind.Val, typeInt)
      val outerY = TypedScopeBinding("y", slot = 1, LocalBindingKind.Val, typeString)
      val innerX = TypedScopeBinding("x", slot = 0, LocalBindingKind.PatternBinding, typeBoolean)

      val envRoot = TypeEnvironment.empty.pushLayer(Vector(outerX, outerY))
      val envInner = envRoot.pushLayer(Vector(innerX))

      // In inner layer: x is shadowed with depth 0 and new type; y is unshadowed with depth 1
      envInner.lookup("x") shouldBe Some((0, innerX))
      envInner.lookup("y") shouldBe Some((1, outerY))

      // In outer layer: x remains unshadowed
      envRoot.lookup("x") shouldBe Some((0, outerX))
      envRoot.lookup("y") shouldBe Some((0, outerY))
    }

    it("should resolve bindings across multiple nested depths") {
      val bindingLevel0 = TypedScopeBinding("a", slot = 0, LocalBindingKind.Val, typeInt)
      val bindingLevel1 = TypedScopeBinding("b", slot = 0, LocalBindingKind.Parameter, typeString)
      val bindingLevel2 = TypedScopeBinding("c", slot = 0, LocalBindingKind.Def, typeBoolean)
      val bindingLevel3 = TypedScopeBinding("d", slot = 0, LocalBindingKind.LazyVal, typeDouble)

      val env = TypeEnvironment.empty
        .pushLayer(Vector(bindingLevel0))
        .pushLayer(Vector(bindingLevel1))
        .pushLayer(Vector(bindingLevel2))
        .pushLayer(Vector(bindingLevel3))

      env.lookup("d") shouldBe Some((0, bindingLevel3))
      env.lookup("c") shouldBe Some((1, bindingLevel2))
      env.lookup("b") shouldBe Some((2, bindingLevel1))
      env.lookup("a") shouldBe Some((3, bindingLevel0))
      env.lookup("unknown") shouldBe None
    }

    it("should maintain independent branch isolation for sibling scopes") {
      val rootBinding = TypedScopeBinding("root", slot = 0, LocalBindingKind.Val, typeInt)
      val baseEnv = TypeEnvironment.empty.pushLayer(Vector(rootBinding))

      val branch1Binding = TypedScopeBinding("case1Var", slot = 1, LocalBindingKind.PatternBinding, typeString)
      val branch1Env = baseEnv.pushLayer(Vector(branch1Binding))

      val branch2Binding = TypedScopeBinding("case2Var", slot = 1, LocalBindingKind.PatternBinding, typeBoolean)
      val branch2Env = baseEnv.pushLayer(Vector(branch2Binding))

      // Branch 1 sees root and branch1Var, but not branch2Var
      branch1Env.lookup("root") shouldBe Some((1, rootBinding))
      branch1Env.lookup("case1Var") shouldBe Some((0, branch1Binding))
      branch1Env.lookup("case2Var") shouldBe None

      // Branch 2 sees root and branch2Var, but not branch1Var
      branch2Env.lookup("root") shouldBe Some((1, rootBinding))
      branch2Env.lookup("case2Var") shouldBe Some((0, branch2Binding))
      branch2Env.lookup("case1Var") shouldBe None

      // Base environment remains unaffected by either branch
      baseEnv.lookup("root") shouldBe Some((0, rootBinding))
      baseEnv.lookup("case1Var") shouldBe None
      baseEnv.lookup("case2Var") shouldBe None
    }
  }
}
