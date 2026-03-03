package software.kes.scaletta.ast

case class Argument(value: Expression,
                    name: Option[Identifier] = None)
