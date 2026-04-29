package software.kes.scaletta.api

trait Setup {
  def methodRegistry: MethodRegistry

  def typeRegistry: TypeRegistry

  def runtimeContextRegistry: RuntimeContextRegistry
}
