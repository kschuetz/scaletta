package software.kes.scaletta.common

sealed trait Interpolator {
  def name: String
}

object Interpolator {

  def fromName(name: String): Interpolator = name match {
    case Raw.name => Raw
    case "f" => F
    case _ => Custom(name)
  }

  /**
   * The "raw" and "f" interpolators will require special handling.
   */

  case class Custom(name: String) extends Interpolator

  case object Raw extends Interpolator {
    final val name: String = "raw"
  }

  case object F extends Interpolator {
    final val name: String = "f"
  }
}
