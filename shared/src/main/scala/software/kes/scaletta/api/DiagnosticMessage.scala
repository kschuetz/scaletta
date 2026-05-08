package software.kes.scaletta.api

import software.kes.scaletta.internal.reporting.Position

case class DiagnosticMessage(message: String,
                             begin: Position,
                             end: Position)
