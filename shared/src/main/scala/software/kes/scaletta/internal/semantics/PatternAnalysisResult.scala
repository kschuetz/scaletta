package software.kes.scaletta.internal.semantics

import software.kes.scaletta.internal.reporting.Pos

final case class PatternAnalysisResult(diagnostics: Vector[Pos[SemanticError]]) {
  def errors: Vector[SemanticError] =
    diagnostics.foldLeft(Vector.empty[SemanticError]) { (result, diagnostic) =>
      result :+ diagnostic.value
    }
}
