package software.kes.scaletta.internal.types

case class TypeArgument[T](parameter: TypeParameter[T],
                           value: Type[T])
