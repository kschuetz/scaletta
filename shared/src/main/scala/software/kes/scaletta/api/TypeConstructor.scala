package software.kes.scaletta.api

import software.kes.scaletta.util.NonEmptyVector

import scala.annotation.tailrec

object TypeConstructor {
  def fromNode[T](node: Type.Constructor[T]): TypeConstructor[T] =
    new TypeConstructor(node.name, node.parameters, Vector.empty)

  def create[T](name: T,
                parameters: NonEmptyVector[TypeParameter[T]]): TypeConstructor[T] =
    new TypeConstructor(name, parameters, Vector.empty)
}

final class TypeConstructor[T] private(val name: T,
                                       val parameters: software.kes.scaletta.util.NonEmptyVector[TypeParameter[T]],
                                       private val applied: Vector[TypeArgument[T]]) {
  private def constructorNode: Type.Constructor[T] = Type.Constructor(name, parameters)
  /**
   * Constructs a type from the given arguments.
   * There must be enough arguments to fill all remaining type parameters, or an IllegalArgumentException
   * will be thrown. Extra arguments will be ignored.
   *
   * Use [[applyArgs]] instead if you want to partially apply arguments.
   */
  def applyAll(args: Type[T]*): Type.Applied[T] = applyAllFromSeq(args)

  /**
   * Constructs a type from the given arguments.
   * There must be enough arguments to fill all remaining type parameters, or an IllegalArgumentException
   * will be thrown. Extra arguments will be ignored.
   *
   * Use [[applyArgs]] instead if you want to partially apply arguments.
   */
  def applyAllFromSeq(args: Seq[Type[T]]): Type.Applied[T] =
    applyArgs(args: _*) match {
      case Right(result) => result
      case Left(tc) =>
        throw new IllegalArgumentException(s"Not enough arguments to construct type (${tc.arity} more needed)")
    }

  /**
   * Applies the given arguments to the type constructor.
   * If all arguments are provided, the resulting type will be returned in a Right.
   * If not enough arguments are provided, a new partially applied TypeConstructor will be
   * returned in a Left.
   */
  def applyArgs(args: Type[T]*): Either[TypeConstructor[T], Type.Applied[T]] = {
    val argsIter = args.iterator
    val totalParamsCount = applied.length + parameters.length

    val builder = Vector.newBuilder[TypeArgument[T]]
    builder ++= applied

    @tailrec
    def go(paramIdx: Int): Either[TypeConstructor[T], Type.Applied[T]] = {
      if (paramIdx < totalParamsCount) {
        if (argsIter.hasNext) {
          val param = parameters(paramIdx - applied.length)
          val arg = argsIter.next()
          builder += TypeArgument(param, arg)
          go(paramIdx + 1)
        } else {
          // not enough
          val remainingParams = NonEmptyVector.from(parameters.drop(paramIdx - applied.length))
          Left(new TypeConstructor(name, remainingParams, builder.result()))
        }
      } else {
        Right(Type.Applied(constructorNode, NonEmptyVector.from(builder.result())))
      }
    }

    go(applied.length)
  }

  def arity: Int = parameters.length

  override def equals(other: Any): Boolean = other match {
    case that: TypeConstructor[T @unchecked] =>
      name == that.name &&
        parameters == that.parameters &&
        applied == that.applied
    case _ => false
  }

  override def hashCode(): Int = {
    var result = name.hashCode()
    result = 31 * result + parameters.hashCode()
    result = 31 * result + applied.hashCode()
    result
  }

  override def toString: String =
    s"TypeConstructor($name, ${parameters.length})"
}
