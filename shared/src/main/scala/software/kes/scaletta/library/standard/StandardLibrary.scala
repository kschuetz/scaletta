package software.kes.scaletta.library.standard

import software.kes.scaletta.api.ScalettaModule

object StandardLibrary {
  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.composite(
      ArithmeticOps.module,
    )
}
