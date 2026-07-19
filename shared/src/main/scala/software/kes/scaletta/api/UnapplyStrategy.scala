package software.kes.scaletta.api

trait UnapplyStrategy {
  def tryUnapply(runtimeContextReader: RuntimeContextReader,
                 argCount: Int,
                 value: Any): UnapplyResult
}

object UnapplyStrategy {
  /**
   * Unapply is not supported (i.e., it always fails).
   */
  def none: UnapplyStrategy = None

  object None extends UnapplyStrategy {
    def tryUnapply(runtimeContextReader: RuntimeContextReader,
                   argCount: Int, value: Any): UnapplyResult =
      UnapplyResult.failure
  }
}

