package software.kes.scaletta.internal.library.standard.typeinfo

import software.kes.scaletta.api.{RuntimeContextReader, UnapplyResult, UnapplyStrategy}

object OptionTypeInfo {
  object SomeTypeInfo extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int,
                   value: Any): UnapplyResult =
      if (argCount == 1) value match {
        case Some(x) => UnapplyResult.success1(x)
        case _ => UnapplyResult.failure
      } else UnapplyResult.failure
  }

  object NoneTypeInfo extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int,
                   value: Any): UnapplyResult =
      if (argCount == 0 && value == None) UnapplyResult.success0
      else UnapplyResult.failure
  }
}
