package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.{CallTarget, RuntimeContextReader}

/**
 * A runtime adapter that allows interpreted function values/closures to be exposed
 * to native higher-order functions as [[CallTarget]].
 */
private[interpreter] final class RuntimeCallTarget(val closure: RuntimeClosure,
                                                   val parameterCount: Int,
                                                   val runtimeContexts: RuntimeContextReader) extends CallTarget {
  private val _argumentValues = new Array[Any](parameterCount)

  private[interpreter] def argumentValues(index: Int): Any = {
    if (index < 0 || index >= parameterCount) {
      throw new IndexOutOfBoundsException(s"Argument index $index is out of bounds for function with $parameterCount parameters")
    }
    _argumentValues(index)
  }

  override def setArgument(index: Int, value: Any): Unit = {
    if (index < 0 || index >= parameterCount) {
      throw new IndexOutOfBoundsException(s"Argument index $index is out of bounds for function with $parameterCount parameters")
    }
    _argumentValues(index) = value
  }
}
