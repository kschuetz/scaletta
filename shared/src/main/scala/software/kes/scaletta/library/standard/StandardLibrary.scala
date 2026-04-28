package software.kes.scaletta.library.standard

import software.kes.scaletta.api.Module

object StandardLibrary {
  lazy val module: Module[Unit] =
    Module.composite(
      ArithmeticOps.module,
    )
}
