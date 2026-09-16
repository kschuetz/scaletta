package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.ImportScope
import software.kes.scaletta.internal.ast.{PreResolutionPhase, TypeAscriptionPhase}
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.internal.semantics.TypeAscriber.{Input, Output}
import software.kes.scaletta.internal.types.TypeNameIndex

object TypeAscriber {
  def create(typeNameIndex: TypeNameIndex,
             importScope: ImportScope): TypeAscriber =
    new TypeAscriber(typeNameIndex, importScope)

  type Input = PreResolutionPhase.Expression[Pos]
  type Output = TypeAscriptionPhase.Expression[Pos]
}

final class TypeAscriber private(typeNameIndex: TypeNameIndex,
                                 importScope: ImportScope) {

  def ascribeTypes(input: Input): Output = ???

}
