package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

object FlatMapFunction {

  trait Strategy[C] extends MapCommon.Strategy[C, C]

  def create[C](strategy: Strategy[C]): ArgumentReader => NativeStep =
    MapCommon.create(strategy)

}
