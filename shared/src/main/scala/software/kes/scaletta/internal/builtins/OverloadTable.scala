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
                typeUniverse.hierarchy.relationshipFor(queryType, formalParam.typ).isSubtype
              case (_, SignatureQueryParameter.Unknown) =>
                true
            }
        }
    }
  }

  def resolveBestMatch(typeUniverse: TypeUniverse,
                       query: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    val candidates = findCandidates(typeUniverse, query)
    candidates match {
      case Nil => Left(ResolutionError.NotFound)
      case bestMatch :: Nil => Right(bestMatch)
      case _ => Left(ResolutionError.Ambiguous)
    }
  }
}
