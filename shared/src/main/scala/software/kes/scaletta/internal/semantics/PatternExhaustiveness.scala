package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.{PatternTypeInfo, ProperType, Type, TypeId}
import software.kes.scaletta.internal.ast.{LinkedPhase, TermSymbolId}
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.types.TypeUniverse

private[semantics] final class PatternExhaustiveness(typeUniverse: TypeUniverse) {
  private val hierarchy = typeUniverse.hierarchy
  private val typeResolver = new PatternTypeResolver(typeUniverse)
  private val wildcard: LinkedPhase.Pattern[Pos] = LinkedPhase.Pattern.Wildcard()

  private sealed trait ConstructorKey

  private final case class LiteralKey(valueType: ProperType[TypeId], value: Any) extends ConstructorKey

  private final case class ProductKey(identity: Any) extends ConstructorKey

  private final case class TupleKey(arity: Int) extends ConstructorKey

  private final case class TypeSliceKey(valueType: ProperType[TypeId]) extends ConstructorKey

  private final case class Constructor(
                                        key: ConstructorKey,
                                        argumentTypes: Vector[ProperType[TypeId]],
                                        matchedType: ProperType[TypeId],
                                        label: String
                                      )

  def analyze(
               scrutineeType: ProperType[TypeId],
               cases: Vector[Pos[LinkedPhase.Case[Pos]]]
             ): PatternAnalysisResult = {
    val processed: (Vector[Vector[LinkedPhase.Pattern[Pos]]], Vector[Pos[SemanticError]]) =
      cases.foldLeft((Vector.empty[Vector[LinkedPhase.Pattern[Pos]]], Vector.empty[Pos[SemanticError]])) {
        case ((matrix, diagnostics), clause) =>
          val pattern = clause.value.pattern.value
          val nextDiagnostics =
            if (isCovered(matrix, Vector(pattern), Vector(scrutineeType))) {
              diagnostics :+ clause.as[SemanticError](SemanticError.UnreachableCase())
            } else {
              diagnostics
            }
          val nextMatrix = clause.value.guard match {
            case Some(_) => matrix
            case None => matrix :+ Vector(pattern)
          }
          (nextMatrix, nextDiagnostics)
      }
    val inexhaustive: Vector[Pos[SemanticError]] =
      if (isCovered(processed._1, Vector(wildcard), Vector(scrutineeType))) {
        Vector.empty
      } else {
        val witness = findMissing(processed._1, Vector(scrutineeType)).fold("_")(_.headOption.fold("_")(identity))
        val position: Pos[SemanticError] = cases.headOption match {
          case Some(first) =>
            val last = cases.last
            Pos(SemanticError.InexhaustiveMatch(s"missing: $witness"), first.begin, last.end)
          case None => Pos(SemanticError.InexhaustiveMatch(s"missing: $witness"), software.kes.scaletta.internal.reporting.CharIndex(0))
        }
        Vector(position)
      }
    PatternAnalysisResult(processed._2 ++ inexhaustive)
  }

  private def isCovered(
                         matrix: Vector[Vector[LinkedPhase.Pattern[Pos]]],
                         query: Vector[LinkedPhase.Pattern[Pos]],
                         targetTypes: Vector[ProperType[TypeId]]
                       ): Boolean = {
    if (targetTypes.isEmpty) {
      matrix.nonEmpty
    } else if (query.isEmpty) {
      false
    } else {
      val targetType = targetTypes.head
      val pattern = unwrapAs(query.head)
      finiteConstructors(targetType) match {
        case Some(constructors) =>
          constructors.foldLeft(true) { (covered, constructor) =>
            if (!covered) {
              false
            } else {
              specialize(pattern, constructor) match {
                case None => true
                case Some(queryPatterns) =>
                  val specializedMatrix = specializeRows(matrix, constructor)
                  isCovered(
                    specializedMatrix,
                    queryPatterns ++ query.tail,
                    constructor.argumentTypes ++ targetTypes.tail
                  )
              }
            }
          }
        case None =>
          openTypeCovered(matrix, pattern, query.tail, targetTypes.tail, targetType)
      }
    }
  }

  private def openTypeCovered(
                               matrix: Vector[Vector[LinkedPhase.Pattern[Pos]]],
                               pattern: LinkedPhase.Pattern[Pos],
                               queryTail: Vector[LinkedPhase.Pattern[Pos]],
                               typeTail: Vector[ProperType[TypeId]],
                               targetType: ProperType[TypeId]
                             ): Boolean = pattern match {
    case LinkedPhase.Pattern.Wildcard() | LinkedPhase.Pattern.Identifier(_) | LinkedPhase.Pattern.Error(_) =>
      val defaultRows = matrix.foldLeft(Vector.empty[Vector[LinkedPhase.Pattern[Pos]]]) {
        case (rows, row) if row.nonEmpty && isTotalOnOpenType(row.head, targetType) => rows :+ row.tail
        case (rows, _) => rows
      }
      isCovered(defaultRows, queryTail, typeTail)
    case LinkedPhase.Pattern.Literal(value) =>
      literalKey(value.value) match {
        case Some(key) =>
          val constructor = Constructor(key, Vector.empty, key.valueType, literalLabel(key.value))
          val specializedMatrix = specializeRows(matrix, constructor)
          isCovered(specializedMatrix, queryTail, typeTail)
        case None => false
      }
    case LinkedPhase.Pattern.Typed(inner, ascription) =>
      ascription.value match {
        case Right(ascribedType) =>
          if (!typeResolver.isCompatible(targetType, ascribedType)) {
            true
          } else {
            val narrowed = hierarchy.greatestLowerBound(targetType, ascribedType)
            val narrowedType = narrowed.asInstanceOf[ProperType[TypeId]]
            val constructor = typeSliceConstructor(narrowedType)
            specialize(pattern, constructor) match {
              case Some(queryPatterns) =>
                isCovered(
                  specializeRows(matrix, constructor),
                  queryPatterns ++ queryTail,
                  constructor.argumentTypes ++ typeTail
                )
              case None => true
            }
          }
        case Left(_) => false
      }
    case LinkedPhase.Pattern.Tuple(elements) =>
      val tupleType = tuplePatternType(elements.size)
      if (targetType == tupleType) {
        val constructor = tupleConstructor(tupleType)
        specialize(pattern, constructor) match {
          case Some(queryPatterns) =>
            isCovered(
              specializeRows(matrix, constructor),
              queryPatterns ++ queryTail,
              constructor.argumentTypes ++ typeTail
            )
          case None => true
        }
      } else {
        if (!typeResolver.isCompatible(targetType, tupleType)) {
          true
        } else {
          val narrowed = hierarchy.greatestLowerBound(targetType, tupleType)
          val constructor = typeSliceConstructor(narrowed.asInstanceOf[ProperType[TypeId]])
          specialize(pattern, constructor) match {
            case Some(queryPatterns) =>
              isCovered(
                specializeRows(matrix, constructor),
                queryPatterns ++ queryTail,
                constructor.argumentTypes ++ typeTail
              )
            case None => true
          }
        }
      }
    case LinkedPhase.Pattern.Product(typeId, args) =>
      typeResolver.productType(typeId.value, targetType) match {
        case Some(productType) =>
          if (!typeResolver.isCompatible(targetType, productType)) {
            true
          } else if (targetType == productType) {
            val constructor = productConstructor(productType, args.size)
            specialize(pattern, constructor) match {
              case Some(queryPatterns) =>
                isCovered(
                  specializeRows(matrix, constructor),
                  queryPatterns ++ queryTail,
                  constructor.argumentTypes ++ typeTail
                )
              case None => true
            }
          } else {
            val narrowed = hierarchy.greatestLowerBound(targetType, productType)
            val constructor = typeSliceConstructor(narrowed.asInstanceOf[ProperType[TypeId]])
            specialize(pattern, constructor) match {
              case Some(queryPatterns) =>
                isCovered(
                  specializeRows(matrix, constructor),
                  queryPatterns ++ queryTail,
                  constructor.argumentTypes ++ typeTail
                )
              case None => true
            }
          }
        case None => false
      }
    case LinkedPhase.Pattern.As(_, inner) =>
      openTypeCovered(matrix, unwrapAs(inner.value), queryTail, typeTail, targetType)
  }

  private def isTotalOnOpenType(
                                 pattern: LinkedPhase.Pattern[Pos],
                                 targetType: ProperType[TypeId]
                               ): Boolean = unwrapAs(pattern) match {
    case LinkedPhase.Pattern.Wildcard() | LinkedPhase.Pattern.Identifier(_) | LinkedPhase.Pattern.Error(_) => true
    case LinkedPhase.Pattern.Typed(inner, ascription) =>
      ascription.value match {
        case Right(ascribedType) =>
          hierarchy.isSubtype(targetType, ascribedType) && isTotalOnOpenType(inner.value, targetType)
        case Left(_) => false
      }
    case LinkedPhase.Pattern.As(_, inner) => isTotalOnOpenType(inner.value, targetType)
    case _ => false
  }

  private def specializeRows(
                              matrix: Vector[Vector[LinkedPhase.Pattern[Pos]]],
                              constructor: Constructor
                            ): Vector[Vector[LinkedPhase.Pattern[Pos]]] =
    matrix.foldLeft(Vector.empty[Vector[LinkedPhase.Pattern[Pos]]]) {
      case (rows, row) if row.nonEmpty =>
        specialize(unwrapAs(row.head), constructor) match {
          case Some(patterns) => rows :+ (patterns ++ row.tail)
          case None => rows
        }
      case (rows, _) => rows
    }

  private def specialize(
                          pattern: LinkedPhase.Pattern[Pos],
                          constructor: Constructor
                        ): Option[Vector[LinkedPhase.Pattern[Pos]]] = pattern match {
    case LinkedPhase.Pattern.Wildcard() | LinkedPhase.Pattern.Identifier(_) | LinkedPhase.Pattern.Error(_) =>
      Some(Vector.fill(constructor.argumentTypes.size)(wildcard))
    case LinkedPhase.Pattern.As(_, inner) => specialize(inner.value, constructor)
    case LinkedPhase.Pattern.Typed(inner, ascription) =>
      ascription.value match {
        case Right(ascribedType) if hierarchy.isSubtype(constructor.matchedType, ascribedType) =>
          specialize(inner.value, constructor)
        case _ => None
      }
    case LinkedPhase.Pattern.Literal(value) =>
      literalKey(value.value) match {
        case Some(key) if constructor.key == key => Some(Vector.empty)
        case Some(key) if constructor.key.isInstanceOf[TypeSliceKey] &&
          hierarchy.isSubtype(key.valueType, constructor.matchedType) => Some(Vector(pattern))
        case _ => None
      }
    case LinkedPhase.Pattern.Tuple(elements) =>
      constructor.key match {
        case TupleKey(arity) if arity == elements.size => Some(elements.map(_.value))
        case TypeSliceKey(_) if constructor.matchedType.isInstanceOf[Type.Tuple[TypeId]] => Some(Vector(pattern))
        case _ => None
      }
    case LinkedPhase.Pattern.Product(typeId, args) =>
      typeResolver.productType(typeId.value, constructor.matchedType) match {
        case Some(productType) =>
          constructor.key match {
            case ProductKey(identity) if productIdentity(productType).contains(identity) &&
              hierarchy.isSubtype(constructor.matchedType, productType) => Some(args.map(_.value))
            case TypeSliceKey(_) if hierarchy.isSubtype(constructor.matchedType, productType) => Some(Vector(pattern))
            case _ => None
          }
        case None => None
      }
  }

  private def finiteConstructors(targetType: ProperType[TypeId]): Option[Vector[Constructor]] = targetType match {
    case Type.Union(types) =>
      Some(types.toVector.foldLeft(Vector.empty[Constructor]) { (result, alternative) =>
        val properType = alternative.asInstanceOf[ProperType[TypeId]]
        result :+ typeSliceConstructor(properType)
      })
    case Type.Tuple(elements) =>
      Some(Vector(tupleConstructor(targetType)))
    case Type.Unit =>
      Some(Vector(Constructor(LiteralKey(CoreTypes.UnitT, ()), Vector.empty, CoreTypes.UnitT, "()")))
    case Type.Singleton(symbol) =>
      Some(Vector(productConstructor(targetType, 0, symbol.toString)))
    case _ => patternTypeInfo(targetType) match {
      case Some(PatternTypeInfo.BooleanDomain) =>
        Some(Vector(
          Constructor(LiteralKey(CoreTypes.BooleanT, true), Vector.empty, CoreTypes.BooleanT, "true"),
          Constructor(LiteralKey(CoreTypes.BooleanT, false), Vector.empty, CoreTypes.BooleanT, "false")
        ))
      case Some(PatternTypeInfo.Product(fields)) =>
        val fieldTypes = substituteFields(fields, targetType)
        Some(Vector(Constructor(
          ProductKey(productIdentity(targetType).getOrElse(targetType)),
          fieldTypes,
          targetType,
          simpleTypeName(targetType)
        )))
      case Some(PatternTypeInfo.Sum(constructors, labels)) =>
        val alternatives = constructors.zipWithIndex.foldLeft(Vector.empty[Constructor]) {
          case (result, (template, index)) =>
            val alternative = substituteType(template, targetType)
            val fields = productFieldTypes(alternative).getOrElse(Vector.empty)
            val label = labels.lift(index).getOrElse(simpleTypeName(alternative))
            result :+ productConstructor(alternative, fields.size, label, fields)
        }
        Some(alternatives)
      case _ => None
    }
  }

  private def tupleConstructor(tupleType: ProperType[TypeId]): Constructor = tupleType match {
    case Type.Tuple(elements) =>
      val argumentTypes = elements.toVector
      Constructor(TupleKey(argumentTypes.size), argumentTypes, tupleType, "tuple")
    case _ => Constructor(TupleKey(0), Vector.empty, tupleType, "tuple")
  }

  private def productConstructor(
                                  productType: ProperType[TypeId],
                                  fallbackArity: Int,
                                  label: String = "",
                                  fallbackFields: Vector[ProperType[TypeId]] = Vector.empty
                                ): Constructor = {
    val fields = if (fallbackFields.nonEmpty || fallbackArity == 0) {
      fallbackFields
    } else {
      productFieldTypes(productType).getOrElse(Vector.fill(fallbackArity)(Type.Top))
    }
    Constructor(
      ProductKey(productIdentity(productType).getOrElse(productType)),
      fields,
      productType,
      if (label.nonEmpty) label else simpleTypeName(productType)
    )
  }

  private def typeSliceConstructor(valueType: ProperType[TypeId]): Constructor =
    Constructor(TypeSliceKey(valueType), Vector(valueType), valueType, simpleTypeName(valueType))

  private def patternTypeInfo(targetType: ProperType[TypeId]): Option[PatternTypeInfo] = targetType match {
    case Type.Nominal(typeId) => typeUniverse.getPatternTypeInfo(typeId)
    case Type.Applied(Type.Constructor(typeId, _), _) => typeUniverse.getPatternTypeInfo(typeId)
    case Type.Singleton(symbol: TermSymbolId.Static) => typeUniverse.getPatternTypeInfo(symbol)
    case _ => None
  }

  private def productFieldTypes(productType: ProperType[TypeId]): Option[Vector[ProperType[TypeId]]] =
    patternTypeInfo(productType) match {
      case Some(PatternTypeInfo.Product(fields)) => Some(substituteFields(fields, productType))
      case _ => None
    }

  private def substituteFields(
                                fields: Vector[ProperType[TypeId]],
                                appliedType: ProperType[TypeId]
                              ): Vector[ProperType[TypeId]] =
    fields.foldLeft(Vector.empty[ProperType[TypeId]]) { (result, field) =>
      result :+ substituteType(field, appliedType)
    }

  private def substituteType(
                              template: ProperType[TypeId],
                              appliedType: ProperType[TypeId]
                            ): ProperType[TypeId] =
    template.substitute(typeArguments(appliedType)).asInstanceOf[ProperType[TypeId]]

  private def typeArguments(targetType: ProperType[TypeId]): Vector[Type[TypeId]] = targetType match {
    case Type.Applied(_, arguments) => arguments.toVector.foldLeft(Vector.empty[Type[TypeId]]) {
      (result, argument) => result :+ argument.value
    }
    case _ => Vector.empty
  }

  private def productIdentity(productType: ProperType[TypeId]): Option[Any] = productType match {
    case Type.Nominal(typeId) => Some(typeId)
    case Type.Applied(Type.Constructor(typeId, _), _) => Some(typeId)
    case Type.Singleton(symbol) => Some(symbol)
    case _ => None
  }

  private def tuplePatternType(arity: Int): ProperType[TypeId] = {
    val elements = Vector.fill(arity)(Type.Top: ProperType[TypeId])
    elements match {
      case first +: second +: rest => Type.tuple(first, second, rest: _*)
      case _ => Type.Top
    }
  }

  private def literalKey(literal: LinkedPhase.Literal[Pos]): Option[LiteralKey] = literal match {
    case LinkedPhase.Literal.IntLiteral(value) => Some(LiteralKey(CoreTypes.IntT, value))
    case LinkedPhase.Literal.LongLiteral(value) => Some(LiteralKey(CoreTypes.LongT, value))
    case LinkedPhase.Literal.FloatLiteral(value) => Some(LiteralKey(CoreTypes.FloatT, value))
    case LinkedPhase.Literal.DoubleLiteral(value) => Some(LiteralKey(CoreTypes.DoubleT, value))
    case LinkedPhase.Literal.True() => Some(LiteralKey(CoreTypes.BooleanT, true))
    case LinkedPhase.Literal.False() => Some(LiteralKey(CoreTypes.BooleanT, false))
    case LinkedPhase.Literal.Null() => Some(LiteralKey(CoreTypes.NullT, null))
    case LinkedPhase.Literal.CharLiteral(value) => Some(LiteralKey(CoreTypes.CharT, value))
    case LinkedPhase.Literal.StringLiteral(value) => Some(LiteralKey(CoreTypes.StringT, value))
    case LinkedPhase.Literal.UnitLiteral() => Some(LiteralKey(CoreTypes.UnitT, ()))
  }

  private def literalLabel(value: Any): String = value match {
    case null => "null"
    case unit: Unit => "()"
    case string: String => s"\"$string\""
    case other => other.toString
  }

  private def findMissing(
                           matrix: Vector[Vector[LinkedPhase.Pattern[Pos]]],
                           targetTypes: Vector[ProperType[TypeId]]
                         ): Option[Vector[String]] = {
    if (targetTypes.isEmpty) {
      if (matrix.isEmpty) Some(Vector.empty) else None
    } else {
      val targetType = targetTypes.head
      finiteConstructors(targetType) match {
        case Some(constructors) =>
          constructors.foldLeft(Option.empty[Vector[String]]) { (found, constructor) =>
            found match {
              case some@Some(_) => some
              case None =>
                val specializedMatrix = specializeRows(matrix, constructor)
                findMissing(specializedMatrix, constructor.argumentTypes ++ targetTypes.tail).map { witness =>
                  val argumentCount = constructor.argumentTypes.size
                  val arguments = witness.take(argumentCount)
                  val remaining = witness.drop(argumentCount)
                  renderConstructor(constructor, arguments) +: remaining
                }
            }
          }
        case None =>
          val defaultRows = matrix.foldLeft(Vector.empty[Vector[LinkedPhase.Pattern[Pos]]]) {
            case (rows, row) if row.nonEmpty && isTotalOnOpenType(row.head, targetType) => rows :+ row.tail
            case (rows, _) => rows
          }
          if (defaultRows.isEmpty) {
            findMissing(Vector.empty, targetTypes.tail).map("_" +: _)
          } else {
            findMissing(defaultRows, targetTypes.tail).map("_" +: _)
          }
      }
    }
  }

  private def renderConstructor(constructor: Constructor, arguments: Vector[String]): String =
    constructor.key match {
      case LiteralKey(_, value) => literalLabel(value)
      case TupleKey(_) => arguments.mkString("(", ", ", ")")
      case ProductKey(_) =>
        if (arguments.isEmpty) {
          constructor.label
        } else if (constructor.label.contains("_")) {
          arguments.foldLeft(constructor.label) { (label, argument) =>
            label.replaceFirst("_", java.util.regex.Matcher.quoteReplacement(argument))
          }
        } else {
          constructor.label + arguments.mkString("(", ", ", ")")
        }
      case TypeSliceKey(_) =>
        if (arguments.isEmpty || arguments == Vector("_")) constructor.label
        else constructor.label + arguments.mkString("(", ", ", ")")
    }

  private def simpleTypeName(valueType: ProperType[TypeId]): String = valueType match {
    case Type.Singleton(symbol: TermSymbolId.Static) =>
      symbol.toString.split("[.$]").lastOption.getOrElse(symbol.toString)
    case Type.Nominal(typeId) => typeId.toString
    case Type.Applied(Type.Constructor(typeId, _), _) => typeId.toString
    case Type.Tuple(_) => "tuple"
    case _ => "_"
  }

  private def unwrapAs(pattern: LinkedPhase.Pattern[Pos]): LinkedPhase.Pattern[Pos] = pattern match {
    case LinkedPhase.Pattern.As(_, inner) => unwrapAs(inner.value)
    case other => other
  }
}
