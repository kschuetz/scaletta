package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{CustomInterpolator, ProperType, TypeId}
import software.kes.scaletta.internal.runtime.CoreTypes

object SInterpolator extends CustomInterpolator {
  def resultType: ProperType[TypeId] = CoreTypes.StringT

  def execute(prefix: String, segments: Vector[CustomInterpolator.Segment]): Any = {
    val output = new StringBuilder(prefix)
    segments.foreach { segment =>
      output.append(segment.value)
      output.append(segment.suffix)
    }
    output.result()
  }
}
