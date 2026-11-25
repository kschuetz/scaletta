package software.kes.scaletta.internal.format

import java.util.{IllegalFormatCodePointException, IllegalFormatConversionException, Locale}
import scala.util.control.NonFatal

object ValueFormatter {
  def create(locale: Locale): ValueFormatter = new ValueFormatter(locale)
}

final class ValueFormatter private(locale: Locale) {

  /**
   * Applies format specifier to the given runtime value.
   * Exception containment guarantees deterministic total execution without throwing.
   */
  def applyFormat(formatSpecifier: FormatSpecifier,
                  value: Any): Either[RuntimeFormattingError, String] =
    if (value == null) {
      formatSpecifier match {
        case g: FormatSpecifier.General =>
          val pattern = g.toPattern
          try {
            val result = java.lang.String.format(locale, pattern, null)
            Right(result)
          } catch {
            case NonFatal(e) =>
              Left(RuntimeFormattingError.GeneralFormattingError(
                specifier = formatSpecifier,
                message = Option(e.getMessage).getOrElse(e.toString),
                cause = Some(e)
              ))
          }
        case nonGeneral =>
          Left(RuntimeFormattingError.NullValueNotAllowed(nonGeneral))
      }
    } else {
      val pattern = formatSpecifier.toPattern
      val boxed = boxValue(value)
      try {
        val result = java.lang.String.format(locale, pattern, boxed)
        Right(result)
      } catch {
        case e: IllegalFormatConversionException =>
          Left(RuntimeFormattingError.IncompatibleType(
            specifier = formatSpecifier,
            actualType = value.getClass.getName,
            message = e.getMessage
          ))
        case e: IllegalFormatCodePointException =>
          Left(RuntimeFormattingError.IllegalCodePoint(
            specifier = formatSpecifier,
            codePoint = e.getCodePoint
          ))
        case _: NullPointerException =>
          Left(RuntimeFormattingError.NullValueNotAllowed(formatSpecifier))
        case NonFatal(e) =>
          Left(RuntimeFormattingError.GeneralFormattingError(
            specifier = formatSpecifier,
            message = Option(e.getMessage).getOrElse(e.toString),
            cause = Some(e)
          ))
      }
    }

  private def boxValue(value: Any): AnyRef =
    value match {
      case null => null
      case bi: scala.math.BigInt => bi.bigInteger
      case bd: scala.math.BigDecimal => bd.bigDecimal
      case ref: AnyRef => ref
      case other => other.asInstanceOf[AnyRef]
    }
}
