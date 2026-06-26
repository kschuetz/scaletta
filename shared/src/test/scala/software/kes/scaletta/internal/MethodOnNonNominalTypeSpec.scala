package software.kes.scaletta.internal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins._
import software.kes.scaletta.internal.symbols.SignatureQuery
import software.kes.scaletta.internal.types.TypeRegistryImpl
import software.kes.scaletta.internal.runtime.CoreTypes

class MethodOnNonNominalTypeSpec extends AnyFunSpec with Matchers {

  describe("Method resolution on non-nominal types") {
    it("should allow registering and resolving methods on Type.Unit") {
      val typeRegistry = new TypeRegistryImpl()
      val typeUniverse = typeRegistry.build()

      val mb = MethodUniverseBuilder.create()
      val methodName = MethodName(ReceiverType.instance(Type.unit), Name("toString"))

      mb.addMethod(methodName, Vector.empty, CoreTypes.StringT, null)

      val mu = mb.build()
      val universe = Universe.create(typeUniverse, mu)

      val query = SignatureQuery.empty
      val result = universe.resolveBestMethod(Type.unit, Name("toString"), query)

      result should be(a[Right[_, _]])
      result.toOption.get.returnType shouldBe CoreTypes.StringT
    }

    it("should allow registering and resolving methods on Type.Tuple") {
      val typeRegistry = new TypeRegistryImpl()
      val typeUniverse = typeRegistry.build()

      val mb = MethodUniverseBuilder.create()
      val tupleType = Type.tuple(CoreTypes.IntT, CoreTypes.StringT)
      val methodName = MethodName(ReceiverType.instance(tupleType), Name("swap"))

      val swappedTupleType = Type.tuple(CoreTypes.StringT, CoreTypes.IntT)
      mb.addMethod(methodName, Vector.empty, swappedTupleType, null)

      val mu = mb.build()
      val universe = Universe.create(typeUniverse, mu)

      val query = SignatureQuery.empty
      val result = universe.resolveBestMethod(tupleType, Name("swap"), query)

      result should be(a[Right[_, _]])
      result.toOption.get.returnType shouldBe swappedTupleType
    }
  }
}
