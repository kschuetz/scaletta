package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.{ProperType, Type, TypeApplier, TypeId}
import software.kes.scaletta.internal.types.{TypeResolutionError, TypeUniverse}

private[semantics] final class PatternTypeResolver(typeUniverse: TypeUniverse) {
  private val hierarchy = typeUniverse.hierarchy

  def productType(
                   typeId: Either[TypeResolutionError, ProperType[TypeId]],
                   targetType: ProperType[TypeId]
                 ): Option[ProperType[TypeId]] = typeId match {
    case Right(resolvedType) => Some(resolvedType)
    case Left(TypeResolutionError.NotAProperType(name)) =>
      constructor(name) match {
        case Some(typeConstructor) =>
          val targetArguments = typeArguments(targetType)
          val arguments = if (targetArguments.size == typeConstructor.parameters.size) {
            targetArguments
          } else {
            Vector.fill(typeConstructor.parameters.size)(Type.Top: Type[TypeId])
          }
          Some(TypeApplier.fromNode(typeConstructor).applyAll(arguments: _*))
        case None => None
      }
    case Left(_) => None
  }

  def isCompatible(left: ProperType[TypeId], right: ProperType[TypeId]): Boolean =
    !isKnownDisjoint(left, right) &&
      (hierarchy.isSubtype(left, right) || hierarchy.greatestLowerBound(left, right) != Type.Bottom)

  private def constructor(name: String): Option[Type.Constructor[TypeId]] = {
    val candidates = typeUniverse.nameIndex.allNames.foldLeft(Vector.empty[Type.Constructor[TypeId]]) {
      (result, fullName) =>
        if (fullName != null && (fullName.name.value == name || fullName.fullName == name)) {
          typeUniverse.nameIndex.get(fullName) match {
            case Some(typeConstructor: Type.Constructor[TypeId]) => result :+ typeConstructor
            case _ => result
          }
        } else {
          result
        }
    }
    candidates match {
      case Vector(typeConstructor) => Some(typeConstructor)
      case _ => None
    }
  }

  private def typeArguments(targetType: ProperType[TypeId]): Vector[Type[TypeId]] = targetType match {
    case Type.Applied(_, arguments) => arguments.toVector.foldLeft(Vector.empty[Type[TypeId]]) {
      (result, argument) => result :+ argument.value
    }
    case _ => Vector.empty
  }

  private def isKnownDisjoint(left: ProperType[TypeId], right: ProperType[TypeId]): Boolean =
    (hierarchy.isSubtype(left, Type.TopValue) && hierarchy.isSubtype(right, Type.TopRef)) ||
      (hierarchy.isSubtype(left, Type.TopRef) && hierarchy.isSubtype(right, Type.TopValue))
}
