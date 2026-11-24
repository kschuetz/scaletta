package software.kes.scaletta.internal.format

sealed trait FormatSpecifierError

object FormatSpecifierError {
  case object LonePercent extends FormatSpecifierError

  final case class InvalidEscape(found: Char) extends FormatSpecifierError

  final case class ConflictingFlags(flag1: Char, flag2: Char) extends FormatSpecifierError

  final case class ConflictingSignFlags(flags: Vector[Char]) extends FormatSpecifierError

  final case class IncompatibleFlag(flag: Char, conversion: Char) extends FormatSpecifierError

  case object WidthOverflow extends FormatSpecifierError

  case object MissingPrecision extends FormatSpecifierError

  case object PrecisionOverflow extends FormatSpecifierError

  final case class PrecisionNotPermitted(conversion: Char) extends FormatSpecifierError

  case object MissingConversion extends FormatSpecifierError

  final case class UnknownConversion(found: Char) extends FormatSpecifierError

  case object MissingDateTimeSuffix extends FormatSpecifierError

  final case class UnknownDateTimeSuffix(found: Char) extends FormatSpecifierError
}
