package software.kes.scaletta.api

case class DiagnosticMessage(message: String,
                             begin: Position,
                             end: Position)
