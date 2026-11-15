package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

object FlatMapFunction {

  trait Strategy[C, R] extends HigherOrderCommon.Strategy[C, R]

  def create[C, R](strategy: Strategy[C, R]): ArgumentReader => NativeStep =
    HigherOrderCommon.create((_, out) => out.asInstanceOf[R])(strategy)

}
