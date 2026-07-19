package software.kes.scaletta.internal.library.standard.typeinfo

import software.kes.scaletta.api.{RuntimeContextReader, UnapplyResult, UnapplyStrategy}

object CollectionTypeInfo {
  object ListTypeInfo extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int,
                   value: Any): UnapplyResult =
      value match {
        case x: List[_] =>
          if (x.lengthCompare(argCount) == 0) UnapplyResult.success(x)
          else UnapplyResult.failure
        case _ => UnapplyResult.failure
      }
  }

  object VectorTypeInfo extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int,
                   value: Any): UnapplyResult =
      value match {
        case x: Vector[_] =>
          if (x.lengthCompare(argCount) == 0) UnapplyResult.success(x)
          else UnapplyResult.failure
        case _ => UnapplyResult.failure
      }
  }
}
