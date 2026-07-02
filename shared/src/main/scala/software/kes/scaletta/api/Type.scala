package software.kes.scaletta.api

import software.kes.scaletta.util.{NonEmptyVector, SetTwoPlus, VectorTwoPlus}

sealed trait Type[+T] {
  def isGround: Boolean
}

sealed trait ConcreteType[T] extends Type[T]

/** A type that can have value-level inhabitants. */
sealed trait ProperType[T] extends ConcreteType[T]

/** A type-level function that requires application to produce a [[ProperType]]. */
sealed trait TypeConstructor[T] extends ConcreteType[T]

object Type {
  def nominal[T](name: T): ProperType[T] = Nominal(name)

  def constructor[T](name: T, parameters: NonEmptyVector[TypeParameter[T]]): TypeConstructor[T] =
    Constructor(name, parameters)

  def applied[T](constructor: Type[T],
                 arg1: TypeArgument[T],
                 moreArgs: TypeArgument[T]*): ProperType[T] =
    Applied(constructor, NonEmptyVector(arg1, moreArgs: _*))

  def function[T](parameters: Type[T]*)
                 (returnType: Type[T]): ProperType[T] =
    Function(parameters.toVector, returnType)

  def intersection[T](t1: ProperType[T], t2: ProperType[T], more: ProperType[T]*): ProperType[T] =
    Intersection(SetTwoPlus(t1, t2, more: _*))

  def tuple[T](t1: Type[T], t2: Type[T], more: Type[T]*): ProperType[T] =
    Tuple(VectorTwoPlus(t1, t2, more: _*))

  def union[T](t1: ProperType[T], t2: ProperType[T], more: ProperType[T]*): ProperType[T] =
    Union(SetTwoPlus(t1, t2, more: _*))

  def unit[T]: ProperType[T] = Unit.asInstanceOf[ProperType[T]]

  def top[T]: ProperType[T] = Top.asInstanceOf[ProperType[T]]

  def topValue[T]: ProperType[T] = TopValue.asInstanceOf[ProperType[T]]

  def topRef[T]: ProperType[T] = TopRef.asInstanceOf[ProperType[T]]

  def bottom[T]: ProperType[T] = Bottom.asInstanceOf[ProperType[T]]

  def bottomRef[T]: ProperType[T] = BottomRef.asInstanceOf[ProperType[T]]

  def variable(paramIndex: Int): Variable = Variable(0, paramIndex)

  case class Nominal[T](name: T) extends ProperType[T] {
    def isGround: Boolean = true
  }

  case class Constructor[T](name: T,
                            parameters: NonEmptyVector[TypeParameter[T]]) extends TypeConstructor[T] {
    def isGround: Boolean = true
  }

  case class Applied[T](constructor: Type[T],
                        arguments: NonEmptyVector[TypeArgument[T]]) extends ProperType[T] {
    def isGround: Boolean = constructor.isGround && arguments.forall(_.value.isGround)
  }

  case class Union[T](types: SetTwoPlus[ProperType[T]]) extends ProperType[T] {
    def isGround: Boolean = types.forall(_.isGround)
  }

  case class Intersection[T](types: SetTwoPlus[ProperType[T]]) extends ProperType[T] {
    def isGround: Boolean = types.forall(_.isGround)
  }

  case class Function[T](parameters: Vector[Type[T]], result: Type[T]) extends ProperType[T] {
    def isGround: Boolean = parameters.forall(_.isGround) && result.isGround
  }

  case object Unit extends ProperType[Nothing] {
    def isGround: Boolean = true
  }

  case class Tuple[T](elements: VectorTwoPlus[Type[T]]) extends ProperType[T] {
    def isGround: Boolean = elements.forall(_.isGround)
  }

  case object Top extends ProperType[Nothing] {
    def isGround: Boolean = true
  }

  case object TopValue extends ProperType[Nothing] {
    def isGround: Boolean = true
  }

  case object TopRef extends ProperType[Nothing] {
    def isGround: Boolean = true
  }

  case object BottomRef extends ProperType[Nothing] {
    def isGround: Boolean = true
  }

  case object Bottom extends ProperType[Nothing] {
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
        case u: Union[T] @unchecked =>
          Union(SetTwoPlus.from(u.types.map(t => go(t).asInstanceOf[ProperType[T]])))
        case i: Intersection[T] @unchecked =>
          Intersection(SetTwoPlus.from(i.types.map(t => go(t).asInstanceOf[ProperType[T]])))
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
