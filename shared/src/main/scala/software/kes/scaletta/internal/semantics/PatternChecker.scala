package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.{PatternTypeInfo, ProperType, Type, TypeId}
import software.kes.scaletta.internal.ast.{LinkedInfo, LinkedPhase, TermSymbolId}
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.types.TypeUniverse

final class PatternChecker private[scaletta](typeUniverse: TypeUniverse) {
  private val hierarchy = typeUniverse.hierarchy
  private val typeResolver = new PatternTypeResolver(typeUniverse)
  private val exhaustiveness = new PatternExhaustiveness(typeUniverse)

  def typePattern(pattern: Pos[LinkedPhase.Pattern[Pos]],
                  targetType: ProperType[TypeId]): Either[SemanticError, Map[String, ProperType[TypeId]]] =
    typePatternValue(pattern.value, targetType)

  def checkValPatternIrrefutable(pattern: Pos[LinkedPhase.Pattern[Pos]],
                                 targetType: ProperType[TypeId]): Option[SemanticError.RefutableValPattern] =
    if (isIrrefutable(pattern.value, targetType)) {
      None
    } else {
      Some(SemanticError.RefutableValPattern(s"Pattern is refutable for type $targetType"))
    }

  def checkMatchExhaustiveness(scrutineeType: ProperType[TypeId],
                               cases: Vector[Pos[LinkedPhase.Case[Pos]]]): Vector[SemanticError] =
    analyzeMatchExhaustiveness(scrutineeType, cases).errors

  def analyzeMatchExhaustiveness(scrutineeType: ProperType[TypeId],
                                 cases: Vector[Pos[LinkedPhase.Case[Pos]]]): PatternAnalysisResult =
    exhaustiveness.analyze(scrutineeType, cases)

  private def typePatternValue(pattern: LinkedPhase.Pattern[Pos],
                               targetType: ProperType[TypeId]): Either[SemanticError, Map[String, ProperType[TypeId]]] = pattern match {
    case LinkedPhase.Pattern.Wildcard() => Right(Map.empty)
    case LinkedPhase.Pattern.Identifier(name) =>
      name.value match {
        case LinkedInfo.Local(_, _, localName, _) => Right(Map(localName -> targetType))
        case LinkedInfo.Error(error) => Left(error)
        case _ => Right(Map.empty)
      }
    case LinkedPhase.Pattern.As(name, inner) =>
      val innerBindings = typePatternValue(inner.value, targetType)
      innerBindings.flatMap { bindings =>
        name.value match {
          case LinkedInfo.Local(_, _, localName, _) =>
            if (bindings.contains(localName)) {
              Left(SemanticError.DuplicateDefinition(localName))
            } else {
              Right(bindings + (localName -> targetType))
            }
          case LinkedInfo.Error(error) => Left(error)
          case _ => Right(bindings)
        }
      }
    case LinkedPhase.Pattern.Literal(value) =>
      literalType(value.value).fold[Either[SemanticError, Map[String, ProperType[TypeId]]]](Right(Map.empty)) {
        case (literalType, _) =>
          if (typeResolver.isCompatible(literalType, targetType)) {
            Right(Map.empty)
          } else {
            Left(SemanticError.TypeMismatch(targetType, literalType))
          }
      }
    case LinkedPhase.Pattern.Typed(inner, ascription) =>
      ascription.value match {
        case Left(_) => typePatternValue(inner.value, targetType)
        case Right(ascribedType) =>
          if (!typeResolver.isCompatible(targetType, ascribedType)) {
            Left(SemanticError.TypeMismatch(targetType, ascribedType))
          } else {
            val narrowed = hierarchy.greatestLowerBound(targetType, ascribedType)
            typePatternValue(inner.value, narrowed.asInstanceOf[ProperType[TypeId]])
          }
      }
    case LinkedPhase.Pattern.Tuple(elements) =>
      tupleElementTypes(targetType, elements.size) match {
        case Some(elementTypes) =>
          typePatterns(elements, elementTypes)
        case None =>
          Left(SemanticError.TypeMismatch(targetType, tuplePatternType(elements.size)))
      }
    case LinkedPhase.Pattern.Product(typeId, args) =>
      typeResolver.productType(typeId.value, targetType) match {
        case None => typePatterns(args, Vector.fill(args.size)(Type.Top))
        case Some(productType) =>
          if (!typeResolver.isCompatible(targetType, productType)) {
            Left(SemanticError.TypeMismatch(targetType, productType))
          } else {
            productFieldTypes(productType) match {
              case Some(fieldTypes) if fieldTypes.size != args.size =>
                Left(SemanticError.TypeMismatch(targetType, productType))
              case Some(fieldTypes) => typePatterns(args, fieldTypes)
              case None => typePatterns(args, Vector.fill(args.size)(Type.Top))
            }
          }
      }
    case LinkedPhase.Pattern.Error(_) => Right(Map.empty)
  }

  private def typePatterns(
                            patterns: Vector[Pos[LinkedPhase.Pattern[Pos]]],
                            targetTypes: Vector[ProperType[TypeId]]
                          ): Either[SemanticError, Map[String, ProperType[TypeId]]] = {
    if (patterns.size != targetTypes.size) {
      Left(SemanticError.TypeMismatch(Type.Top, Type.Top))
    } else {
      patterns.zip(targetTypes).foldLeft(
        Right(Map.empty[String, ProperType[TypeId]]): Either[SemanticError, Map[String, ProperType[TypeId]]]
      ) { case (result, (pattern, targetType)) =>
        result.flatMap { bindings =>
          typePatternValue(pattern.value, targetType).flatMap { nextBindings =>
            mergeBindings(bindings, nextBindings)
          }
        }
      }
    }
  }

  private def mergeBindings(left: Map[String, ProperType[TypeId]],
                            right: Map[String, ProperType[TypeId]]): Either[SemanticError, Map[String, ProperType[TypeId]]] = {
    right.foldLeft(Right(left): Either[SemanticError, Map[String, ProperType[TypeId]]]) {
      case (result, (name, valueType)) =>
        result.flatMap { bindings =>
          if (bindings.contains(name)) {
            Left(SemanticError.DuplicateDefinition(name))
          } else {
            Right(bindings + (name -> valueType))
          }
        }
    }
  }

  private def isIrrefutable(pattern: LinkedPhase.Pattern[Pos],
                            targetType: ProperType[TypeId]): Boolean = pattern match {
    case LinkedPhase.Pattern.Wildcard() => true
    case LinkedPhase.Pattern.Identifier(_) => true
    case LinkedPhase.Pattern.As(_, inner) => isIrrefutable(inner.value, targetType)
    case LinkedPhase.Pattern.Literal(value) =>
      value.value match {
        case LinkedPhase.Literal.UnitLiteral() => targetType == Type.Unit
        case LinkedPhase.Literal.Null() => targetType == Type.BottomRef
        case _ => false
      }
    case LinkedPhase.Pattern.Typed(inner, ascription) =>
      ascription.value match {
        case Right(ascribedType) =>
          hierarchy.isSubtype(targetType, ascribedType) && isIrrefutable(inner.value, targetType)
        case Left(_) => false
      }
    case LinkedPhase.Pattern.Tuple(elements) =>
      tupleElementTypesForIrrefutability(targetType, elements.size) match {
        case Some(elementTypes) =>
          elements.zip(elementTypes).foldLeft(true) { case (valid, (element, elementType)) =>
            valid && isIrrefutable(element.value, elementType)
          }
        case None => false
      }
    case LinkedPhase.Pattern.Product(typeId, args) =>
      typeResolver.productType(typeId.value, targetType) match {
        case Some(productType) if hierarchy.isSubtype(targetType, productType) =>
          productFieldTypes(productType) match {
            case Some(fieldTypes) if fieldTypes.size == args.size =>
              args.zip(fieldTypes).foldLeft(true) { case (valid, (argument, fieldType)) =>
                valid && isIrrefutable(argument.value, fieldType)
              }
            case _ => false
          }
        case _ => false
      }
    case LinkedPhase.Pattern.Error(_) => false
  }

  private def tupleElementTypes(targetType: ProperType[TypeId],
                                arity: Int): Option[Vector[ProperType[TypeId]]] = targetType match {
    case Type.Tuple(elements) if elements.size == arity => Some(elements.toVector)
    case Type.Top | Type.TopRef => Some(Vector.fill(arity)(Type.Top))
    case Type.Union(types) =>
      val alternatives = types.toVector
      val tuples = alternatives.foldLeft(Vector.empty[Vector[ProperType[TypeId]]]) {
        case (result, Type.Tuple(elements)) if elements.size == arity => result :+ elements.toVector
        case (result, _) => result
      }
      if (tuples.size != alternatives.size) {
        None
      } else {
        tuples.headOption.fold(Option.empty[Vector[ProperType[TypeId]]]) { first =>
          val combined = tuples.tail.foldLeft(first) { (current, next) =>
            current.zip(next).foldLeft(Vector.empty[ProperType[TypeId]]) { case (result, (left, right)) =>
              result :+ hierarchy.leastUpperBound(left, right).asInstanceOf[ProperType[TypeId]]
            }
          }
          Some(combined)
        }
      }
    case _ => None
  }

  private def tupleElementTypesForIrrefutability(targetType: ProperType[TypeId],
                                                 arity: Int): Option[Vector[ProperType[TypeId]]] = targetType match {
    case Type.Tuple(elements) if elements.size == arity => Some(elements.toVector)
    case Type.Union(types) =>
      val alternatives = types.toVector
      val allTupleTypes = alternatives.foldLeft(Vector.empty[Vector[ProperType[TypeId]]]) {
        case (result, Type.Tuple(elements)) if elements.size == arity => result :+ elements.toVector
        case (result, _) => result
      }
      if (allTupleTypes.size != alternatives.size || allTupleTypes.isEmpty) {
        None
      } else {
        val combined = allTupleTypes.tail.foldLeft(allTupleTypes.head) { (current, next) =>
          current.zip(next).foldLeft(Vector.empty[ProperType[TypeId]]) { case (result, (left, right)) =>
            result :+ hierarchy.leastUpperBound(left, right).asInstanceOf[ProperType[TypeId]]
          }
        }
        Some(combined)
      }
    case _ => None
  }

  private def tuplePatternType(arity: Int): ProperType[TypeId] = {
    val elements = Vector.fill(arity)(Type.Top: ProperType[TypeId])
    elements match {
      case first +: second +: rest => Type.tuple(first, second, rest: _*)
      case _ => Type.Top
    }
  }

  private def productFieldTypes(productType: ProperType[TypeId]): Option[Vector[ProperType[TypeId]]] =
    patternTypeInfo(productType) match {
      case Some(PatternTypeInfo.Product(fields)) =>
        val arguments = typeArguments(productType)
        Some(fields.foldLeft(Vector.empty[ProperType[TypeId]]) { (result, field) =>
          result :+ field.substitute(arguments).asInstanceOf[ProperType[TypeId]]
        })
      case _ => None
    }

  private def patternTypeInfo(targetType: ProperType[TypeId]): Option[PatternTypeInfo] = targetType match {
    case Type.Nominal(typeId) => typeUniverse.getPatternTypeInfo(typeId)
    case Type.Applied(Type.Constructor(typeId, _), _) => typeUniverse.getPatternTypeInfo(typeId)
    case Type.Singleton(symbol: TermSymbolId.Static) => typeUniverse.getPatternTypeInfo(symbol)
    case _ => None
  }

  private def typeArguments(targetType: ProperType[TypeId]): Vector[Type[TypeId]] = targetType match {
    case Type.Applied(_, arguments) => arguments.toVector.foldLeft(Vector.empty[Type[TypeId]]) {
      (result, argument) => result :+ argument.value
    }
    case _ => Vector.empty
  }

  private def isCompatible(left: ProperType[TypeId], right: ProperType[TypeId]): Boolean =
    typeResolver.isCompatible(left, right)

  private def literalType(literal: LinkedPhase.Literal[Pos]): Option[(ProperType[TypeId], Any)] = literal match {
    case LinkedPhase.Literal.IntLiteral(value) => Some((CoreTypes.IntT, value))
    case LinkedPhase.Literal.LongLiteral(value) => Some((CoreTypes.LongT, value))
    case LinkedPhase.Literal.FloatLiteral(value) => Some((CoreTypes.FloatT, value))
    case LinkedPhase.Literal.DoubleLiteral(value) => Some((CoreTypes.DoubleT, value))
    case LinkedPhase.Literal.True() => Some((CoreTypes.BooleanT, true))
    case LinkedPhase.Literal.False() => Some((CoreTypes.BooleanT, false))
    case LinkedPhase.Literal.Null() => Some((CoreTypes.NullT, null))
    case LinkedPhase.Literal.CharLiteral(value) => Some((CoreTypes.CharT, value))
    case LinkedPhase.Literal.StringLiteral(value) => Some((CoreTypes.StringT, value))
    case LinkedPhase.Literal.UnitLiteral() => Some((CoreTypes.UnitT, ()))
  }
}

object PatternChecker {
  def create(typeUniverse: TypeUniverse): PatternChecker =
    new PatternChecker(typeUniverse)
}
