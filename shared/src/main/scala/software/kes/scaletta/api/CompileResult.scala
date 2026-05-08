package software.kes.scaletta.api

case class CompileResult(value: Either[CompileErrors, CompiledExpression],
                         warnings: Vector[DiagnosticMessage],
                         hints: Vector[DiagnosticMessage]) {
  def isSuccess: Boolean = value.isRight

  def isFailure: Boolean = value.isLeft

  def errors: Vector[DiagnosticMessage] = value match {
    case Left(errors) => errors.errors.toVector
    case Right(_) => Vector.empty
  }
}
