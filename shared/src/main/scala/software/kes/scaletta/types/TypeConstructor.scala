package software.kes.scaletta.types

import software.kes.scaletta.util.{ArityList, EmptyArityList, NonEmptyArityList}

import scala.annotation.tailrec

object TypeConstructor {
  def create[T](name: T,
                parameters: NonEmptyArityList[TypeParameter[T]]): TypeConstructor[T] =
    new TypeConstructor(name, parameters, Nil)
}

final class TypeConstructor[T] private(val name: T,
                                       val parameters: NonEmptyArityList[TypeParameter[T]],
                                       private val applied: List[TypeArgument[T]]) {
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

    @tailrec
    def go(acc: List[TypeArgument[T]],
           params: ArityList[TypeParameter[T]]): Either[TypeConstructor[T], Type.Applied[T]] =
      params match {
        case remaining@NonEmptyArityList(head, tail) =>
          if (argsIter.hasNext) {
            val arg = argsIter.next()
            go(TypeArgument(head, arg) :: acc, tail)
          } else {
            // not enough
            Left(new TypeConstructor(name, remaining, acc))
          }
        case EmptyArityList => Right {
          NonEmptyArityList.tryFrom(acc.reverse) match {
            case Some(result) => Type.Applied(name, result)
            case None =>
              // This should never happen
              throw new AssertionError("No type arguments")
          }
        }
      }

    go(applied, parameters)
  }

  def arity: Int = parameters.arity

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
    s"TypeConstructor($name, ${parameters.arity})"
}
