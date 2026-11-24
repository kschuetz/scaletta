package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{InterpolatorId, ProperType, TypeId}
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.semantics.SemanticError

sealed trait LinkedInterpolator {
  def resultType: ProperType[TypeId]
}

object LinkedInterpolator {
  final case class Custom(id: InterpolatorId,
                          resultType: ProperType[TypeId]) extends LinkedInterpolator

  final case class Error(error: SemanticError) extends LinkedInterpolator {
    def resultType: ProperType[TypeId] = CoreTypes.StringT
  }

  case object Raw extends LinkedInterpolator {
    def resultType: ProperType[TypeId] = CoreTypes.StringT
  }

  case object F extends LinkedInterpolator {
    def resultType: ProperType[TypeId] = CoreTypes.StringT
  }
}
