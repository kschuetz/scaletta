package software.kes.scaletta.api

import software.kes.scaletta.util.{NonEmptyVector, SetTwoPlus, VectorTwoPlus}

sealed trait Type[+T] {
  def isGround: Boolean
}

sealed trait ConcreteType[T] extends Type[T]

object Type {
  def nominal[T](name: T): ConcreteType[T] = Nominal(name)

  def constructor[T](name: T, parameters: NonEmptyVector[TypeParameter[T]]): ConcreteType[T] =
    Constructor(name, parameters)

  def applied[T](constructor: Type[T],
                 arg1: TypeArgument[T],
                 moreArgs: TypeArgument[T]*): ConcreteType[T] =
    Applied(constructor, NonEmptyVector(arg1, moreArgs: _*))

  def function[T](parameters: Type[T]*)
                 (returnType: Type[T]): ConcreteType[T] =
    Function(parameters.toVector, returnType)

  def intersection[T](t1: Type[T], t2: Type[T], more: Type[T]*): ConcreteType[T] =
    Intersection(SetTwoPlus(t1, t2, more: _*))

  def tuple[T](t1: Type[T], t2: Type[T], more: Type[T]*): ConcreteType[T] =
    Tuple(VectorTwoPlus(t1, t2, more: _*))

  def union[T](t1: Type[T], t2: Type[T], more: Type[T]*): ConcreteType[T] =
    Union(SetTwoPlus(t1, t2, more: _*))

  def unit[T]: ConcreteType[T] = Unit.asInstanceOf[ConcreteType[T]]

  def top[T]: ConcreteType[T] = Top.asInstanceOf[ConcreteType[T]]

  def topValue[T]: ConcreteType[T] = TopValue.asInstanceOf[ConcreteType[T]]

  def topRef[T]: ConcreteType[T] = TopRef.asInstanceOf[ConcreteType[T]]

  def bottom[T]: ConcreteType[T] = Bottom.asInstanceOf[ConcreteType[T]]

  def bottomRef[T]: ConcreteType[T] = BottomRef.asInstanceOf[ConcreteType[T]]

  def variable(paramIndex: Int): Variable = Variable(0, paramIndex)

  case class Nominal[T](name: T) extends ConcreteType[T] {
    def isGround: Boolean = true
  }

  case class Constructor[T](name: T,
                            parameters: NonEmptyVector[TypeParameter[T]]) extends ConcreteType[T] {
    def isGround: Boolean = true
  }

  case class Applied[T](constructor: Type[T],
                        arguments: NonEmptyVector[TypeArgument[T]]) extends ConcreteType[T] {
    def isGround: Boolean = constructor.isGround && arguments.forall(_.value.isGround)
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

  case object Unit extends ConcreteType[Nothing] {
    def isGround: Boolean = true
  }

  case class Tuple[T](elements: VectorTwoPlus[Type[T]]) extends ConcreteType[T] {
    def isGround: Boolean = elements.forall(_.isGround)
  }

  case object Top extends ConcreteType[Nothing] {
    def isGround: Boolean = true
  }

  case object TopValue extends ConcreteType[Nothing] {
    def isGround: Boolean = true
  }

  case object TopRef extends ConcreteType[Nothing] {
    def isGround: Boolean = true
  }

  case object BottomRef extends ConcreteType[Nothing] {
    def isGround: Boolean = true
  }

  case object Bottom extends ConcreteType[Nothing] {
    def isGround: Boolean = true
  }

  case class Variable(scopeIndex: Int,
                      paramIndex: Int) extends Type[Nothing] {
    def isGround: Boolean = false
  }

  implicit class TypeOps[T](val self: Type[T]) extends AnyVal {
    def substitute(arguments: IndexedSeq[Type[T]], scopeIndex: Int = 0): Type[T] = {
      def go(t: Type[T]): Type[T] = t match {
        case v: Variable if v.scopeIndex == scopeIndex =>
          if (v.paramIndex >= 0 && v.paramIndex < arguments.length) arguments(v.paramIndex)
          else v
        case Nominal(name) => Nominal(name)
        case Constructor(name, params) => Constructor(name, params)
        case a: Applied[T] @unchecked =>
          Applied[T](go(a.constructor), a.arguments.map(arg => arg.copy(value = go(arg.value))))
        case Union(types) =>
          Union(SetTwoPlus.from(types.map(go)))
        case Intersection(types) =>
          Intersection(SetTwoPlus.from(types.map(go)))
        case Function(params, result) =>
          Function(params.map(go), go(result))
        case Tuple(elements) =>
          Tuple(VectorTwoPlus.from(elements.map(go)))
        case Unit => Unit
        case Top => Top
        case TopValue => TopValue
        case TopRef => TopRef
        case BottomRef => BottomRef
        case Bottom => Bottom
        case other => other
      }

      go(self)
    }
  }
}
