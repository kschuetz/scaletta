package software.kes.scaletta.api

trait RuntimeContextRegistry {
  def createRuntimeContextType(): RuntimeContextId
}
