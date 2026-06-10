package software.kes.scaletta.testsupport

import software.kes.scaletta.api.{RuntimeContextId, RuntimeContextReader}

object emptyContextReader extends RuntimeContextReader {
  def readRuntimeContext[A](runtimeContextId: RuntimeContextId): A =
    throw new UnsupportedOperationException("No contexts")
}
