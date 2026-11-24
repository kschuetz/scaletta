package software.kes.scaletta.internal.format

sealed trait RuntimeFormattingError

object RuntimeFormattingError {

  final case class IncompatibleType(specifier: FormatSpecifier,
                                    actualType: String,
                                    message: String) extends RuntimeFormattingError

  final case class IllegalCodePoint(specifier: FormatSpecifier,
                                    codePoint: Int) extends RuntimeFormattingError

  final case class NullValueNotAllowed(specifier: FormatSpecifier) extends RuntimeFormattingError

  final case class GeneralFormattingError(specifier: FormatSpecifier,
                                          message: String,
                                          cause: Option[Throwable]) extends RuntimeFormattingError
}
