package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

object FilterFunction {

  trait Strategy[C] extends HigherOrderCommon.Strategy[C, (Any, Boolean)]

  def create[C](strategy: Strategy[C]): ArgumentReader => NativeStep =
    HigherOrderCommon.create((in, keep) => (in, keep.asInstanceOf[Boolean]))(strategy)

}
