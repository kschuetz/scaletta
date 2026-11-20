package software.kes.scaletta.internal.builtins

import software.kes.scaletta.internal.symbols.{SignatureQuery, SignatureQueryParameter}
import software.kes.scaletta.internal.types.{TypeConversion, TypeConversionGraph, TypeUniverse}

object OverloadTable {
  def resolveBestMatch(typeUniverse: TypeUniverse,
                       candidates: List[NativeFunctionDefinition]): Either[ResolutionError, NativeFunctionDefinition] = {
    candidates match {
      case Nil => Left(ResolutionError.NotFound)
      case bestMatch :: Nil => Right(bestMatch)
      case _ =>
        val bests = candidates.filter { c1 =>
          !candidates.exists(c2 => c1 != c2 && isMoreSpecific(typeUniverse, c2, c1))
        }

        bests.distinct match {
          case Nil => Left(ResolutionError.NotFound)
          case best :: Nil => Right(best)
          case _ => Left(ResolutionError.Ambiguous)
        }
    }
  }

  private def isMoreSpecific(typeUniverse: TypeUniverse,
                             v1: NativeFunctionDefinition,
                             v2: NativeFunctionDefinition): Boolean = {
    if (v1 == v2) false
    else if (v1.paramGroups.size != v2.paramGroups.size) false
    else {
      final case class Acc(compatible: Boolean, hasAdvantage: Boolean)

      val result = v1.paramGroups.zip(v2.paramGroups).foldLeft(Acc(compatible = true, hasAdvantage = false)) {
        case (acc, (g1, g2)) =>
          if (!acc.compatible) acc
          else {
            (g1.variadic, g2.variadic) match {
              case (None, None) =>
                val compatible = g1.params.size == g2.params.size &&
                  g1.params.zip(g2.params).forall { case (p1, p2) =>
                    typeUniverse.hierarchy.isSubtype(p1.typ, p2.typ)
                  }
                val advantage = compatible && g1.params.zip(g2.params).exists { case (p1, p2) =>
                  p1.typ != p2.typ
                }
                Acc(compatible, acc.hasAdvantage || advantage)

              case (None, Some(v2Var)) =>
                val compatible = g1.params.size >= g2.params.size && {
                  val (prefix, suffix) = g1.params.splitAt(g2.params.size)
                  prefix.zip(g2.params).forall { case (p1, p2) =>
                    typeUniverse.hierarchy.isSubtype(p1.typ, p2.typ)
                  } &&
                    suffix.forall(p => typeUniverse.hierarchy.isSubtype(p.typ, v2Var.typ))
                }
                Acc(compatible, acc.hasAdvantage || compatible)

              case (Some(_), None) =>
                Acc(compatible = false, hasAdvantage = false)

              case (Some(v1Var), Some(v2Var)) =>
                val compatible = g1.params.size == g2.params.size &&
                  g1.params.zip(g2.params).forall { case (p1, p2) =>
                    typeUniverse.hierarchy.isSubtype(p1.typ, p2.typ)
                  } &&
                  typeUniverse.hierarchy.isSubtype(v1Var.typ, v2Var.typ)
                val advantage = compatible && (g1.params.zip(g2.params).exists { case (p1, p2) =>
                  p1.typ != p2.typ
                } || v1Var.typ != v2Var.typ)
                Acc(compatible, acc.hasAdvantage || advantage)
            }
          }
      }

      result.compatible && result.hasAdvantage
    }
  }
}

final case class OverloadTable(variations: List[NativeFunctionDefinition]) {
  def findCandidates(typeUniverse: TypeUniverse,
                     query: SignatureQuery): List[NativeFunctionDefinition] = {
    variations.filter { variation =>
      variation.paramGroups.size == query.groups.size &&
        variation.paramGroups.zip(query.groups).forall { case (formalGroup, queryGroup) =>
          formalGroup.variadic match {
            case Some(v) =>
              queryGroup.parameters.size >= formalGroup.params.size && {
                val (fixedQuery, variadicQuery) = queryGroup.parameters.splitAt(formalGroup.params.size)
                formalGroup.params.zip(fixedQuery).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                } &&
                  variadicQuery.forall {
                    case SignatureQueryParameter.OfType(queryType) =>
                      typeUniverse.hierarchy.isSubtype(queryType, v.typ)
                    case SignatureQueryParameter.Unknown =>
                      true
                  }
              }
            case None =>
              formalGroup.params.size == queryGroup.parameters.size &&
                formalGroup.params.zip(queryGroup.parameters).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                }
          }
        }
    }
  }

  def resolveBestMatch(typeUniverse: TypeUniverse,
                       query: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    val candidates = findCandidates(typeUniverse, query)
    if (candidates.nonEmpty) {
      OverloadTable.resolveBestMatch(typeUniverse, candidates)
    } else {
      val conversionCandidates = findCandidatesWithConversions(typeUniverse, query)
      if (conversionCandidates.isEmpty) Left(ResolutionError.NotFound)
      else {
        val withCosts = conversionCandidates.map(c => (c, calculateTotalCost(typeUniverse, query, c)))
        val minCost = withCosts.map(_._2).min
        val bestByCost = withCosts.filter(_._2 == minCost).map(_._1)
        OverloadTable.resolveBestMatch(typeUniverse, bestByCost)
      }
    }
  }

  def findCandidatesWithConversions(typeUniverse: TypeUniverse,
                                    query: SignatureQuery): List[NativeFunctionDefinition] = {
    variations.filter { variation =>
      variation.paramGroups.size == query.groups.size &&
        variation.paramGroups.zip(query.groups).forall { case (formalGroup, queryGroup) =>
          formalGroup.variadic match {
            case Some(v) =>
              queryGroup.parameters.size >= formalGroup.params.size && {
                val (fixedQuery, variadicQuery) = queryGroup.parameters.splitAt(formalGroup.params.size)
                formalGroup.params.zip(fixedQuery).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ) ||
                      TypeConversionGraph.conversionRelation(queryType, formalParam.typ) != TypeConversion.None
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                } &&
                  variadicQuery.forall {
                    case SignatureQueryParameter.OfType(queryType) =>
                      typeUniverse.hierarchy.isSubtype(queryType, v.typ) ||
                        TypeConversionGraph.conversionRelation(queryType, v.typ) != TypeConversion.None
                    case SignatureQueryParameter.Unknown =>
                      true
                  }
              }
            case None =>
              formalGroup.params.size == queryGroup.parameters.size &&
                formalGroup.params.zip(queryGroup.parameters).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ) ||
                      TypeConversionGraph.conversionRelation(queryType, formalParam.typ) != TypeConversion.None
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                }
          }
        }
    }
  }

  private def calculateTotalCost(typeUniverse: TypeUniverse,
                                 query: SignatureQuery,
                                 variation: NativeFunctionDefinition): Int = {
    variation.paramGroups.zip(query.groups).foldLeft(0) { case (acc, (formalGroup, queryGroup)) =>
      formalGroup.variadic match {
        case Some(v) =>
          val (fixedQuery, variadicQuery) = queryGroup.parameters.splitAt(formalGroup.params.size)
          val fixedCost = formalGroup.params.zip(fixedQuery).foldLeft(0) {
            case (acc2, (formalParam, SignatureQueryParameter.OfType(queryType))) =>
              val cost = if (typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)) 0
              else TypeConversionGraph.conversionRelation(queryType, formalParam.typ).cost
              acc2 + cost
            case (acc2, (_, SignatureQueryParameter.Unknown)) =>
              acc2
          }
          val variadicCost = variadicQuery.foldLeft(0) {
            case (acc2, SignatureQueryParameter.OfType(queryType)) =>
              val cost = if (typeUniverse.hierarchy.isSubtype(queryType, v.typ)) 0
              else TypeConversionGraph.conversionRelation(queryType, v.typ).cost
              acc2 + cost
            case (acc2, SignatureQueryParameter.Unknown) =>
              acc2
          }
          acc + fixedCost + variadicCost
        case None =>
          acc + formalGroup.params.zip(queryGroup.parameters).foldLeft(0) {
            case (acc2, (formalParam, SignatureQueryParameter.OfType(queryType))) =>
              val cost = if (typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)) 0
              else TypeConversionGraph.conversionRelation(queryType, formalParam.typ).cost
              acc2 + cost
            case (acc2, (_, SignatureQueryParameter.Unknown)) =>
              acc2
          }
      }
    }
  }
}
