package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{CustomInterpolator, FunctionImpl}
import software.kes.scaletta.internal.runtime.CoreTypes

object SInterpolator {
  lazy val definition: CustomInterpolator = {
    val impl: FunctionImpl = FunctionImpl.objectResult { argReader =>
      val output = new StringBuilder
      val count = argReader.argCount
      var i = 0
      while (i < count) {
        output.append(argReader.read(i))
        i += 1
      }
      output.result()
    }

    CustomInterpolator(CoreTypes.StringT, impl)
  }
}
