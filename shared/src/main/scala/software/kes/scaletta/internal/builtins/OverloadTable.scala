package software.kes.scaletta.internal.builtins

import software.kes.scaletta.internal.symbols.{SignatureQuery, SignatureQueryParameter}
import software.kes.scaletta.internal.types.TypeUniverse

case class OverloadTable(variations: List[NativeFunctionDefinition]) {
  def findCandidates(typeUniverse: TypeUniverse,
                     query: SignatureQuery): List[NativeFunctionDefinition] = {
    variations.filter { variation =>
      variation.paramGroups.size == query.groups.size &&
        variation.paramGroups.zip(query.groups).forall { case (formalGroup, queryGroup) =>
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

  def resolveBestMatch(typeUniverse: TypeUniverse,
                       query: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    val candidates = findCandidates(typeUniverse, query)
    OverloadTable.resolveBestMatch(typeUniverse, candidates)
  }
}

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
      val allSubtypes = v1.paramGroups.zip(v2.paramGroups).forall { case (g1, g2) =>
        g1.params.size == g2.params.size &&
          g1.params.zip(g2.params).forall { case (p1, p2) =>
            typeUniverse.hierarchy.isSubtype(p1.typ, p2.typ)
          }
      }

      if (!allSubtypes) false
      else {
        // v1 is more specific if it's a subtype in at least one position
        v1.paramGroups.zip(v2.paramGroups).exists { case (g1, g2) =>
          g1.params.zip(g2.params).exists { case (p1, p2) =>
            p1.typ != p2.typ
          }
        }
      }
    }
  }
}
