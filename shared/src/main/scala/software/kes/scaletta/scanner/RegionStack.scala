package software.kes.scaletta.scanner

object RegionStack {
  def empty: RegionStack = new RegionStack(Nil, 0)
}

/**
 * We will keep track of interpolated string depth as an optimization:
 * if no interpolated strings are present, we can skip the O(n) search for them.
 */
final class RegionStack private(private val regions: List[RegionAttributes],
                                private val interpolatedStringDepth: Int) {
  def peek: Option[RegionAttributes] = regions.headOption

  def enter(regionAttributes: RegionAttributes): RegionStack =
    regionAttributes.regionType match {
      case RegionType.InterpolatedString =>
        new RegionStack(regionAttributes :: regions, interpolatedStringDepth + 1)
      case _ => new RegionStack(regionAttributes :: regions, interpolatedStringDepth)
    }

  def exit(regionType: RegionType): RegionStack =
    regions match {
      case x :: xs =>
        if (x.regionType == regionType) {
          val updatedInterpolatedStringDepth = if (regionType == RegionType.InterpolatedString) {
            interpolatedStringDepth - 1
          } else interpolatedStringDepth
          new RegionStack(xs, updatedInterpolatedStringDepth)
        } else this
      case Nil => this
    }

  def findFirstInterpolatedString: Option[RegionAttributes] =
    if (interpolatedStringDepth > 0) {
      regions.find(_.regionType == RegionType.InterpolatedString)
    } else None

  def dropUntilInterpolatedString: RegionStack =
    if (interpolatedStringDepth > 0) {
      regions.dropWhile(_.regionType != RegionType.InterpolatedString) match {
        case _ :: xs => new RegionStack(xs, interpolatedStringDepth - 1)
        case Nil => RegionStack.empty
      }
    } else this
}
