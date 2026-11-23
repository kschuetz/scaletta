package software.kes.scaletta.api

import software.kes.scaletta.api.CustomInterpolator.Segment

object CustomInterpolator {
  case class Segment(value: Any,
                     suffix: String)
}

trait CustomInterpolator {
  def resultType: ProperType[TypeId]

  def execute(prefix: String, segments: Vector[Segment]): Any
}
