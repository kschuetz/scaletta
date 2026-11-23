package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.{Name, ScalettaModule}
import software.kes.scaletta.internal.interpolators.SInterpolator

object Interpolators {
  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.interpolatorsOnly { registry =>
      registry.registerInterpolator(Name("s"), SInterpolator.definition)
    }
}
