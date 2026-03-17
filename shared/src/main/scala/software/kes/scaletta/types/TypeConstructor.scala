package software.kes.scaletta.types

import software.kes.scaletta.util.NonEmptyArityList

case class TypeConstructor[T](name: T,
                              parameters: NonEmptyArityList[TypeParameter[T]])
