package software.kes.scaletta.internal

import software.kes.scaletta.internal.builtins.MethodUniverse
import software.kes.scaletta.internal.interpolators.InterpolatorRegistryImpl
import software.kes.scaletta.internal.types.TypeUniverse

object TestUniverse {
  def create(typeUniverse: TypeUniverse,
             methodUniverse: MethodUniverse): Universe =
    Universe.create(typeUniverse, methodUniverse, InterpolatorRegistryImpl.create().build())
}
