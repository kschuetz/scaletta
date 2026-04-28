package software.kes.scaletta.api

trait ScalettaRegistry {
  def methodRegistry: MethodRegistry

  def typeRegistry: TypeRegistry

  def runtimeContextRegistry: RuntimeContextRegistry
}
