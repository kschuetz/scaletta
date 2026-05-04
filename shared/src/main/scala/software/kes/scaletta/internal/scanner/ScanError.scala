package software.kes.scaletta.internal.scanner

sealed trait ScanError

object ScanError {
  case object UnclosedComment extends ScanError

  case object UnclosedCharacterLiteral extends ScanError

  case object UnclosedStringLiteral extends ScanError

  case object UnclosedMultiLineString extends ScanError

  case object UnclosedQuotedIdentifier extends ScanError

  case object EmptyCharacterLiteral extends ScanError

  case object EmptyQuotedIdentifier extends ScanError

  case object InvalidEscapeCharacter extends ScanError

  case object IllegalSeparator extends ScanError

  case object InvalidLiteralNumber extends ScanError

  case object IntegerNumberTooLarge extends ScanError

  case object FloatingPointNumberTooLarge extends ScanError

  case object FloatingPointPrecisionTooLarge extends ScanError

  case object FloatingPointPrecisionTooSmall extends ScanError

  case object IdentifierTooLong extends ScanError

  case object InvalidCharacter extends ScanError

  case object UnbalancedBraces extends ScanError
}
