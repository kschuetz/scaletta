package software.kes.scaletta.api

import software.kes.scaletta.util.NonEmptyVector

import scala.annotation.tailrec

object TypeApplier {
  def fromNode[T](node: Type.Constructor[T]): TypeApplier[T] =
    new TypeApplier(node, node.parameters, Vector.empty)

  def create[T](name: T,
                parameters: NonEmptyVector[TypeParameter[T]]): TypeApplier[T] =
    new TypeApplier(Type.Constructor(name, parameters), parameters, Vector.empty)

  def createFromType[T](target: Type[T],
                        parameters: NonEmptyVector[TypeParameter[T]]): TypeApplier[T] =
    new TypeApplier(target, parameters, Vector.empty)
}

final class TypeApplier[T] private(val target: Type[T],
                                   val parameters: software.kes.scaletta.util.NonEmptyVector[TypeParameter[T]],
                                   private val applied: Vector[TypeArgument[T]]) {

  def name: T = target match {
    case Type.Nominal(n) => n
    case Type.Constructor(n, _) => n
    case _ => throw new IllegalStateException("Target is not a named type")
  }

  /**
   * Constructs a type from the given arguments.
   * There must be enough arguments to fill all remaining type parameters, or an IllegalArgumentException
   * will be thrown. Extra arguments will be ignored.
   *
   * Use [[applyArgs]] instead if you want to partially apply arguments.
   */
  def applyAll(args: Type[T]*): ConcreteType[T] = applyAllFromSeq(args)

  /**
   * Constructs a type from the given arguments.
   * There must be enough arguments to fill all remaining type parameters, or an IllegalArgumentException
   * will be thrown. Extra arguments will be ignored.
   *
   * Use [[applyArgs]] instead if you want to partially apply arguments.
   */
  def applyAllFromSeq(args: Seq[Type[T]]): ConcreteType[T] =
    applyArgs(args: _*) match {
      case Right(result) => result
      case Left(tc) =>
        throw new IllegalArgumentException(s"Not enough arguments to construct type (${tc.arity} more needed)")
    }

  /**
   * Applies the given arguments to the type constructor.
   * If all arguments are provided, the resulting type will be returned in a Right.
   * If not enough arguments are provided, a new partially applied TypeApplier will be
   * returned in a Left.
   */
  def applyArgs(args: Type[T]*): Either[TypeApplier[T], ConcreteType[T]] = {
    val argsIter = args.iterator
    val totalParamsCount = applied.length + parameters.length

    val builder = Vector.newBuilder[TypeArgument[T]]
    builder ++= applied

    @tailrec
    def go(paramIdx: Int): Either[TypeApplier[T], ConcreteType[T]] = {
      if (paramIdx < totalParamsCount) {
        if (argsIter.hasNext) {
          val param = parameters(paramIdx - applied.length)
          val arg = argsIter.next()
          builder += TypeArgument(param, arg)
          go(paramIdx + 1)
        } else {
          // not enough
          val remainingParams = NonEmptyVector.from(parameters.drop(paramIdx - applied.length))
          Left(new TypeApplier(target, remainingParams, builder.result()))
        }
      } else {
        val arguments = builder.result()
        val argValues = arguments.map(_.value)
        val result = target match {
          case Type.Constructor(_, _) =>
            Type.Applied(target, NonEmptyVector.from(arguments))
          case _ =>
            target.substitute(argValues) match {
              case ct: ConcreteType[T @unchecked] => ct
              case _ =>
                Type.Applied(target, NonEmptyVector.from(arguments))
            }
        }
        Right(result)
      }
    }

    go(applied.length)
  }

  def arity: Int = parameters.length

  override def equals(other: Any): Boolean = other match {
    case that: TypeApplier[T @unchecked] =>
      target == that.target &&
        parameters == that.parameters &&
        applied == that.applied
    case _ => false
  }

  override def hashCode(): Int = {
    var result = target.hashCode()
    result = 31 * result + parameters.hashCode()
    result = 31 * result + applied.hashCode()
    result
  }

  override def toString: String = target match {
    case Type.Constructor(n, _) => s"TypeApplier($n, ${parameters.length})"
    case _ => s"TypeApplier($target, ${parameters.length})"
  }
}
