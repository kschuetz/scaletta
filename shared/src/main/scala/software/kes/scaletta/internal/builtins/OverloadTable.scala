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
}
