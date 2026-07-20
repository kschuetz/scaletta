package software.kes.scaletta.api

trait UnapplyStrategy {
  def tryUnapply(runtimeContextReader: RuntimeContextReader,
                 argCount: Int,
                 value: Any): UnapplyResult
}

object UnapplyStrategy {
  def noUnapply: UnapplyStrategy = NoUnapply

  def unapplyZero(fn: Any => Boolean): UnapplyStrategy =
    new UnapplyZero(fn)

  def unapplyOne(fn: Any => UnapplyResult): UnapplyStrategy =
    new UnapplyOne(fn)

  def unapplyDynamic(fn: (Int, Any) => UnapplyResult): UnapplyStrategy =
    new UnapplyN(fn)

  def unapplySeq(fn: Seq[_] => Boolean): UnapplyStrategy =
    new UnapplySeq(fn)

  private object NoUnapply extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int, value: Any): UnapplyResult =
      UnapplyResult.failure
  }

  private class UnapplyZero(fn: Any => Boolean) extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int, value: Any): UnapplyResult =
      if (argCount == 0 && fn(value)) UnapplyResult.success0 else UnapplyResult.failure
  }

  private class UnapplyOne(fn: Any => UnapplyResult) extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int,
                   value: Any): UnapplyResult =
      if (argCount == 1) fn(value) else UnapplyResult.failure
  }

  private class UnapplyN(fn: (Int, Any) => UnapplyResult) extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int,
                   value: Any): UnapplyResult = fn(argCount, value)
  }

  private class UnapplySeq(fn: Seq[_] => Boolean) extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int,
                   value: Any): UnapplyResult =
      value match {
        case seq: Seq[_] =>
          if (fn(seq) && seq.lengthCompare(argCount) == 0) UnapplyResult.success(seq)
          else UnapplyResult.failure
      }
  }

}
