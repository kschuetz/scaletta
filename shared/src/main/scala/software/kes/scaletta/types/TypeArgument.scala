package software.kes.scaletta.types

case class TypeArgument[T](parameter: TypeParameter[T],
                           value: Type[T])
