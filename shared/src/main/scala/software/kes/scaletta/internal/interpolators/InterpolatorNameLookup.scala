package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{InterpolatorId, Name}

trait InterpolatorNameLookup {
  def getInterpolatorByName(name: Name): Option[InterpolatorId]
}
