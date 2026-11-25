package software.kes.scaletta.api

import software.kes.scaletta.internal.format.{FormatSpecifier, RuntimeFormattingError}

trait ValueFormatterErrorHandler {
  def handleError(formatSpecifier: FormatSpecifier,
                  value: Any,
                  error: RuntimeFormattingError): String
}

object ValueFormatterErrorHandler {
  val default: ValueFormatterErrorHandler = ThrowException

  /**
   * If a formatting error occurs, throws an exception and halts execution.
   * The exception thrown will be of type [[RuntimeFormattingException]]
   */
  def throwException: ValueFormatterErrorHandler = ThrowException

  /**
   * If a formatting error occurs, ignores the error and returns a blank string
   *
   * @return
   */
  def ignore: ValueFormatterErrorHandler = Ignore

  /**
   * If a formatting error occurs, ignores the error, and instead returns
   * the unformatted "toString" on the value
   */
  def insertUnformatted: ValueFormatterErrorHandler = InsertUnformatted

  /**
   * If a formatting error occurs, inserts a custom string (e.g., "[ERROR]")
   */
  def insertCustomString(customValue: String): ValueFormatterErrorHandler =
    new InsertCustomString(customValue)

  object ThrowException extends ValueFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String = {
      throw new RuntimeFormattingException("Formatting exception", error)
    }
  }

  object Ignore extends ValueFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String =
      ""
  }

  object InsertUnformatted extends ValueFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String =
      String.valueOf(value)
  }

  final class InsertCustomString(customValue: String) extends ValueFormatterErrorHandler {
    def handleError(formatSpecifier: FormatSpecifier,
                    value: Any,
                    error: RuntimeFormattingError): String =
      customValue
  }

}
