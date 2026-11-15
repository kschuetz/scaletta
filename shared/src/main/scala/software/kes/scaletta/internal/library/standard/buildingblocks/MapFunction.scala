package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

object MapFunction {
  trait Strategy[C] extends HigherOrderCommon.Strategy[C, Any]

  def create[C](strategy: Strategy[C]): ArgumentReader => NativeStep =
    HigherOrderCommon.create((_, out) => out)(strategy)
}
