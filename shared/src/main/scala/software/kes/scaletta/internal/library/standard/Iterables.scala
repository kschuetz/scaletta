package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.higherorder.CommonHigherOrderMethods
import software.kes.scaletta.internal.library.standard.higherorder.CommonHigherOrderMethods.FlatMap

object Iterables {

  def registerMethods(registry: MethodRegistry,
                      standardTypes: StandardTypes)
                     (constructor: Type.Constructor[TypeId],
                      base: IterableBase): Unit = {

    val higherOrderImpls = CommonHigherOrderMethods.Implementations(
      filterImpl = Some(base.filterImpl),
      mapImpl = Some(base.mapImpl),
      flatMapImpl = Some(FlatMap(base.flatMapImpl, Some(standardTypes.IterableOnceT)))
    )

    CommonHigherOrderMethods.registerMethods(registry, standardTypes)(constructor, higherOrderImpls)
  }
}
