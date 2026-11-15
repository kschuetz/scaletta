package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

object FlatMapFunction {

  trait Strategy[C] extends HigherOrderCommon.Strategy[C, C]

  def create[C](strategy: Strategy[C]): ArgumentReader => NativeStep =
    HigherOrderCommon.create((_, out) => out.asInstanceOf[C])(strategy)

}
