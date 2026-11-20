package software.kes.scaletta.api

object ParameterGroup {
  def single(params: FormalParameter*): Vector[ParameterGroup] = Vector(ParameterGroup(params.toVector))

  def variadic(initialParams: Vector[FormalParameter],
               variadicParam: VariadicParameter): ParameterGroup =
    ParameterGroup(initialParams, Some(variadicParam))
}

case class ParameterGroup(params: Vector[FormalParameter],
                          variadic: Option[VariadicParameter] = None)
