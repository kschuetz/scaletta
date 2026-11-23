package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{InterpolatorId, ProperType, TypeId}
import software.kes.scaletta.internal.runtime.CoreTypes

sealed trait LinkedInterpolator {
  def resultType: ProperType[TypeId]
}

object LinkedInterpolator {
  case class Custom(id: InterpolatorId,
                    resultType: ProperType[TypeId]) extends LinkedInterpolator

  case object Raw extends LinkedInterpolator {
    def resultType: ProperType[TypeId] = CoreTypes.StringT
  }

  case object F extends LinkedInterpolator {
    def resultType: ProperType[TypeId] = CoreTypes.StringT
  }
}
