package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{CustomInterpolator, InterpolatorId}

object CustomInterpolatorLookup {
  val empty: CustomInterpolatorLookup = new CustomInterpolatorLookup {
    def getCustomInterpolator(id: InterpolatorId): CustomInterpolator =
      throw new NoSuchElementException(s"Unknown interpolator id: $id")

    def getCustomInterpolator(name: String): Option[LinkedInterpolator.Custom] = None
  }
}

trait CustomInterpolatorLookup {
  def getCustomInterpolator(id: InterpolatorId): CustomInterpolator

  def getCustomInterpolator(name: String): Option[LinkedInterpolator.Custom]
}
