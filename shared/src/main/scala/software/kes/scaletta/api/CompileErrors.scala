package software.kes.scaletta.api

import software.kes.scaletta.util.NonEmptyVector

case class CompileErrors(errors: NonEmptyVector[DiagnosticMessage])
