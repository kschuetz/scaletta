package software.kes.scaletta.types

import software.kes.scaletta.util.{NonEmptyArityList, SetTwoPlus}

sealed trait Type[T]

object Type {
  case class Nominal[T](name: T) extends Type[T]

  case class Applied[T](constructorName: T,
                        arguments: NonEmptyArityList[TypeArgument[T]]) extends Type[T]

  case class Union[T](types: SetTwoPlus[Type[T]]) extends Type[T]

  case class Intersection[T](types: SetTwoPlus[Type[T]]) extends Type[T]
}
