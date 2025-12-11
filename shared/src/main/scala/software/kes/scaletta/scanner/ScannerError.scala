package software.kes.scaletta.scanner

sealed trait ScannerError

object ScannerError {
  case object UnclosedComment extends ScannerError

  case object UnclosedCharacterLiteral extends ScannerError

  case object UnclosedStringLiteral extends ScannerError

  case object UnclosedMultiLineString extends ScannerError

  case object UnclosedQuotedIdentifier extends ScannerError

  case object EmptyCharacterLiteral extends ScannerError

  case object EmptyQuotedIdentifier extends ScannerError

  case object InvalidEscapeCharacter extends ScannerError

  case object IllegalSeparator extends ScannerError

  case object InvalidLiteralNumber extends ScannerError

  case object IntegerNumberTooLarge extends ScannerError

  case object FloatingPointNumberTooLarge extends ScannerError

  case object FloatingPointPrecisionTooLarge extends ScannerError

  case object FloatingPointPrecisionTooSmall extends ScannerError

  case object IdentifierTooLong extends ScannerError
}
