package software.kes.scaletta.internal.types

import software.kes.scaletta.api.Type

case class TypeArgument[T](parameter: TypeParameter[T],
                           value: Type[T])
