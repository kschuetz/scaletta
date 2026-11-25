package software.kes.scaletta.api

import software.kes.scaletta.internal.format.{FormatSpecifier, RuntimeFormattingError}

trait StringFormatterErrorHandler {
  def handleError(formatSpecifier: FormatSpecifier,
                  value: Any,
                  error: RuntimeFormattingError): String
}

object StringFormatterErrorHandler {
  val default: StringFormatterErrorHandler = ThrowException

  /**
   * If a formatting error occurs, throws an exception and halts execution.
   * The exception thrown will be of type [[RuntimeFormattingException]]
   */
  def throwException: StringFormatterErrorHandler = ThrowException

  /**
   * If a formatting error occurs, ignores the error and returns a blank string
   *
   * @return
   */
  def ignore: StringFormatterErrorHandler = Ignore

  /**
   * If a formatting error occurs, ignores the error, and instead returns
   * the unformatted "toString" on the value
   */
  def insertUnformatted: StringFormatterErrorHandler = InsertUnformatted

  /**
   * If a formatting error occurs, inserts a custom string (e.g., "[ERROR]")
   */
  def insertCustomString(customValue: String): StringFormatterErrorHandler =
    new InsertCustomString(customValue)

  object ThrowException extends StringFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String = {
      throw new RuntimeFormattingException("Formatting exception", error)
    }
  }

  object Ignore extends StringFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String =
      ""
  }

  object InsertUnformatted extends StringFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String =
      String.valueOf(value)
  }

  final class InsertCustomString(customValue: String) extends StringFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String =
      customValue
  }

}
