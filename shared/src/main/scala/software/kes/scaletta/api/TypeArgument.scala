package software.kes.scaletta.api

case class TypeArgument[T](parameter: TypeParameter[T],
                           value: Type[T])
