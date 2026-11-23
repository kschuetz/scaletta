package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{CustomInterpolator, InterpolatorId}

trait CustomInterpolatorLookup {
  def getCustomInterpolator(id: InterpolatorId): CustomInterpolator
}
