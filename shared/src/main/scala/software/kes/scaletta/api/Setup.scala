package software.kes.scaletta.api

import software.kes.scaletta.internal.library.standard.StandardTypes

trait Setup {
  def methodRegistry: MethodRegistry

  def typeRegistry: TypeRegistry

  def runtimeContextRegistry: RuntimeContextRegistry

  def standardTypes: StandardTypes
}
