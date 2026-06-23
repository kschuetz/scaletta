package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.ScalettaModule

object StandardLibrary {
  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.composite(
      ArithmeticOps.module,
      ComparisonOps.module,
      EqualityOps.module,
      Math.module,
      Collections.module,
    )
}
