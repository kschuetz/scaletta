package software.kes.scaletta.api

import software.kes.scaletta.internal.types.TypeArgument
import software.kes.scaletta.util.{NonEmptyVector, SetTwoPlus}

sealed trait Type[+T] {
  def isGround: Boolean
}

sealed trait ConcreteType[T] extends Type[T]

object Type {
  case class Nominal[T](name: T) extends ConcreteType[T] {
    def isGround: Boolean = true
  }

  case class Applied[T](constructorName: T,
                        arguments: NonEmptyVector[TypeArgument[T]]) extends ConcreteType[T] {
    def isGround: Boolean = arguments.forall(_.value.isGround)
  }

  case class Union[T](types: SetTwoPlus[Type[T]]) extends ConcreteType[T] {
    def isGround: Boolean = types.forall(_.isGround)
  }

  case class Intersection[T](types: SetTwoPlus[Type[T]]) extends ConcreteType[T] {
    def isGround: Boolean = types.forall(_.isGround)
  }

  case class Variable(scopeIndex: Int,
                      paramIndex: Int) extends Type[Nothing] {
    def isGround: Boolean = false
  }

  def variable(paramIndex: Int): Variable = Variable(0, paramIndex)
}
