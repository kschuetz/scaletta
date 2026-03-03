package software.kes.scaletta.ast

case class Argument(value: Expression,
                    name: Option[Identifier] = None)

case class ArgumentGroup(arguments: Vector[Argument],
                         splat: Option[Argument] = None)
