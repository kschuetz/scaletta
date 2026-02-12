package software.kes.scaletta.scanner

sealed trait RegionType {
  def newlinesEnabled: Boolean
}

object RegionType {
  case object Parens extends RegionType {
    def newlinesEnabled: Boolean = false
  }

  case object Brackets extends RegionType {
    def newlinesEnabled: Boolean = false
  }

  case object Braces extends RegionType {
    def newlinesEnabled: Boolean = true
  }

  case object Case extends RegionType {
    def newlinesEnabled: Boolean = false
  }

  case class InterpolatedString(multiLine: Boolean,
                                isRaw: Boolean) extends RegionType {
    def newlinesEnabled: Boolean = multiLine
  }

  case object InterpolatedEscape extends RegionType {
    def newlinesEnabled: Boolean = true
  }
}
