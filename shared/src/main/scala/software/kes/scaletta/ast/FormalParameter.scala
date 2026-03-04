package software.kes.scaletta.ast

case class FormalParameter(name: Identifier,
                           typ: TypeIdentifier,
                           default: Option[Expression] = None)

case class FormalParameterGroup(parameters: Vector[FormalParameter],
                                variadic: Option[FormalParameter] = None)
