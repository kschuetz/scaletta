package software.kes.scaletta.scanner

sealed trait RegionType

object RegionType {
  case object Parens extends RegionType

  case object Brackets extends RegionType

  case object Braces extends RegionType

  case object Case extends RegionType

  case object InterpolatedString extends RegionType

  case object InterpolatedEscape extends RegionType

  case object Portal extends RegionType
}

sealed trait RegionAttributes {
  def newlinesEnabled: Boolean

  def regionType: RegionType
}

object RegionAttributes {
  case object Parens extends RegionAttributes {
    def newlinesEnabled: Boolean = false

    def regionType: RegionType = RegionType.Parens
  }

  case object Brackets extends RegionAttributes {
    def newlinesEnabled: Boolean = false

    def regionType: RegionType = RegionType.Brackets
  }

  case object Braces extends RegionAttributes {
    def newlinesEnabled: Boolean = true

    def regionType: RegionType = RegionType.Braces
  }

  case object Case extends RegionAttributes {
    def newlinesEnabled: Boolean = false

    def regionType: RegionType = RegionType.Case
  }

  case class InterpolatedString(multiLine: Boolean,
                                isRaw: Boolean) extends RegionAttributes {
    def newlinesEnabled: Boolean = multiLine

    def regionType: RegionType = RegionType.InterpolatedString
  }

  case object InterpolatedEscape extends RegionAttributes {
    def newlinesEnabled: Boolean = true

    def regionType: RegionType = RegionType.InterpolatedEscape
  }

  case object Portal extends RegionAttributes {
    def newlinesEnabled: Boolean = true

    def regionType: RegionType = RegionType.Portal
  }
}
