package software.kes.scaletta.api

trait InterpolatorRegistry {
  def registerInterpolator(name: Name, interpolator: CustomInterpolator): Unit
}
