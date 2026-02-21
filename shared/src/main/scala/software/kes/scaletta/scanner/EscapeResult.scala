package software.kes.scaletta.scanner

/**
 * Represents the result of scanning an escape sequence.
 */
sealed trait EscapeResult

object EscapeResult {

  /**
   * Successfully scanned an escape sequence.
   *
   * @param value the escaped character
   */
  case class Success(value: Char) extends EscapeResult

  /**
   * Encountered an invalid escape sequence.
   *
   * @param error the specific lexical error
   */
  case class Error(error: ScannerError) extends EscapeResult

  /**
   * Encountered a physical line boundary (newline or carriage return) during scanning.
   * Indicates that the literal should be synchronized and terminated.
   */
  case object Boundary extends EscapeResult
}
