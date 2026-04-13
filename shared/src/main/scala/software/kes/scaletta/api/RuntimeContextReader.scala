package software.kes.scaletta.api

trait RuntimeContextReader {
  def readRuntimeContext[A](runtimeContextId: RuntimeContextId): A
}
