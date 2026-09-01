package software.kes.scaletta.internal.preresolver

import software.kes.scaletta.api.ImportScope
import software.kes.scaletta.internal.ast.{ParsingPhase, PreResolutionPhase}
import software.kes.scaletta.internal.builtins.MethodUniverse
import software.kes.scaletta.internal.preresolver.PreResolver.{Input, Output}
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.internal.types.TypeUniverse

object PreResolver {
  def create(typeUniverse: TypeUniverse,
             methodUniverse: MethodUniverse,
             importScope: ImportScope): PreResolver =
    new PreResolver(typeUniverse, methodUniverse, importScope)

  type Input = ParsingPhase.Expression[Pos]
  type Output = PreResolutionPhase.Expression[Pos]
}

final class PreResolver private(typeUniverse: TypeUniverse,
                                methodUniverse: MethodUniverse,
                                importScope: ImportScope) {
  def preResolve(input: Input): Output = ???
}
