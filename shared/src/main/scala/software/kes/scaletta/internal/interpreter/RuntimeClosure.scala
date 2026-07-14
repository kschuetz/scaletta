package software.kes.scaletta.internal.interpreter

/**
 * A runtime representation of a closure, combining a user function index and a captured frame.
 */
private[interpreter] object RuntimeClosure {
  def apply(functionIndex: Int, capturedFrame: CapturedFrame): RuntimeClosure =
    new RuntimeClosure(functionIndex, capturedFrame)
}

private[interpreter] final class RuntimeClosure(val functionIndex: Int,
                                                val capturedFrame: CapturedFrame)
