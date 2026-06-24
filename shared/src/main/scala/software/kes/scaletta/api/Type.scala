package software.kes.scaletta.api

import software.kes.scaletta.util.{NonEmptyVector, SetTwoPlus}

sealed trait Type[+T] {
  def isGround: Boolean
}

sealed trait ConcreteType[T] extends Type[T]

object Type {
  def nominal[T](name: T): ConcreteType[T] = Nominal(name)

  def applied[T](constructorName: T,
                 arg1: TypeArgument[T],
                 moreArgs: TypeArgument[T]*): ConcreteType[T] =
    Applied(constructorName, NonEmptyVector(arg1, moreArgs: _*))

  def function[T](parameters: Type[T]*)
                 (returnType: Type[T]): ConcreteType[T] =
    Function(parameters.toVector, returnType)

  def intersection[T](t1: Type[T], t2: Type[T], more: Type[T]*): ConcreteType[T] =
    Intersection(SetTwoPlus(t1, t2, more: _*))

  def union[T](t1: Type[T], t2: Type[T], more: Type[T]*): ConcreteType[T] =
    Union(SetTwoPlus(t1, t2, more: _*))

  def variable(paramIndex: Int): Variable = Variable(0, paramIndex)

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

  case class Function[T](parameters: Vector[Type[T]], result: Type[T]) extends ConcreteType[T] {
    def isGround: Boolean = parameters.forall(_.isGround) && result.isGround
  }

  case class Variable(scopeIndex: Int,
                      paramIndex: Int) extends Type[Nothing] {
    def isGround: Boolean = false
  }
}
