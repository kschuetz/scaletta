package software.kes.scaletta.scanner

sealed trait Interpolator {
  def name: String
}

object Interpolator {

  def fromName(name: String): Interpolator = name match {
    case ScannerConstants.Raw => Raw
    case "f" => F
    case _ => Custom(name)
  }


  /**
   * The "raw" and "f" interpolators will required special handling.
   */

  case class Custom(name: String) extends Interpolator

  case object Raw extends Interpolator {
    def name: String = ScannerConstants.Raw
  }

  case object F extends Interpolator {
    def name: String = "f"
  }
}
