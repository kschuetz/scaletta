package software.kes.scaletta.internal.format

import scala.scalajs.js

/**
 * Factory and companion object for Scala.js [[ValueFormatter]].
 */
object ValueFormatter {

  /**
   * Constructs a default invariant [[ValueFormatter]] instance for Scala.js.
   *
   * Why: In Scala.js environments, `java.util.Locale` is not available without pulling in
   * extensive emulation libraries. Providing a parameterless factory method creates an invariant
   * (POSIX / ROOT style) formatter with zero third-party dependencies and minimal footprint.
   */
  def create(): ValueFormatter = new ValueFormatter()
}

/**
 * ValueFormatter implementation for Scala.js.
 *
 * =Architectural Compromises & Differences from JVM Version=
 *
 * On the JVM, `ValueFormatter` delegates string formatting directly to `java.lang.String.format`
 * and `java.util.Formatter`, parameterized by a `java.util.Locale`.
 *
 * In the Scala.js environment, the following design decisions and compromises were made to balance
 * utility (template formatting features) against cost (JS bundle size, execution speed, zero dependencies):
 *
 * 1. '''Zero JVM Formatter & Locale Dependencies''':
 * Linking `java.lang.String.format`, `java.util.Formatter`, or `java.util.Locale` in Scala.js
 * pulls in heavy runtime emulation machinery, significantly bloating compiled JS artifacts.
 * This implementation provides pure-Scala and native JS runtime formatting without any third-party
 * or heavy JVM standard library dependencies.
 *
 * 2. '''Invariant / Root Locale Default''':
 * `ValueFormatter.create()` operates deterministically in an invariant default locale (similar to
 * `Locale.ROOT` / `Locale.US`), using ASCII digits, standard grouping separators (`,`), and standard
 * English calendar names.
 *
 * 3. '''General & Character Conversions (`%s`, `%S`, `%b`, `%B`, `%h`, `%H`, `%c`, `%C`)''':
 * Full 100% feature parity with the JVM implementation, supporting precision truncation,
 * upper-casing, width alignment, and Unicode code point range validation.
 *
 * 4. '''Integral Conversions (`%d`, `%o`, `%x`, `%X`)''':
 * Full feature parity supporting `Byte`, `Short`, `Int`, `Long`, `BigInt`, and `java.math.BigInteger`.
 * Includes sign formats (`+`, space, enclose negative `()`), grouping separators (`,`), alternate form (`#`),
 * and alignment / zero-padding (`%05d`, `%-5d`, `%5d`).
 *
 * 5. '''Floating-Point Conversions (`%f`, `%e`, `%E`, `%g`, `%G`, `%a`, `%A`)''':
 *    - Supported: Decimal float (`%f`), scientific notation (`%e`, `%E`), and general scientific (`%g`, `%G`)
 *      are implemented using native JavaScript `Number` formatting routines (`toFixed`, `toExponential`, `toPrecision`),
 *      supporting custom precision, grouping separators, sign formats, and zero-padding.
 *    - Excluded: Hexadecimal floating-point conversions (`%a`, `%A`) are omitted due to the disproportionate
 *      code size and complexity of IEEE 754 bit-level hex mantissa extraction compared to its utility in templates.
 *      Attempting to format with `%a` or `%A` returns a [[RuntimeFormattingError.IncompatibleType]].
 *
 * 6. '''Date & Time Conversions (`%t`, `%T`)''':
 *    - Supported Input Types: `Long` (epoch milliseconds), `scala.scalajs.js.Date`, and `java.util.Date`.
 *    - Supported Specifiers: ISO and standard numeric formats (`%tF`, `%tT`, `%tR`, `%tr`, `%tD`, `%tY`, `%ty`,
 *      `%tC`, `%tm`, `%td`, `%te`, `%tj`, `%tH`, `%tk`, `%tI`, `%tl`, `%tM`, `%tS`, `%tL`, `%tN`, `%tp`,
 *      `%ts`, `%tQ`, `%tc`).
 *    - Month and weekday names (`%tB`, `%tb`, `%th`, `%tA`, `%ta`) are formatted using standard English names.
 *    - Arbitrary JVM-specific `java.time.temporal.TemporalAccessor` instances requiring reflection or
 *      zone databases are not supported in Scala.js.
 *
 * 7. '''Total Error Containment''':
 * All format applications return `Either[RuntimeFormattingError, String]` without throwing runtime exceptions.
 */
final class ValueFormatter private() {

  /**
   * Formats a given runtime value according to the supplied [[FormatSpecifier]].
   */
  def applyFormat(formatSpecifier: FormatSpecifier,
                  value: Any): Either[RuntimeFormattingError, String] =
    if (value == null) {
      formatSpecifier match {
        case g: FormatSpecifier.General =>
          Right(formatGeneral(g, null))
        case nonGeneral =>
          Left(RuntimeFormattingError.NullValueNotAllowed(nonGeneral))
      }
    } else {
      formatSpecifier match {
        case g: FormatSpecifier.General =>
          Right(formatGeneral(g, value))

        case c: FormatSpecifier.Character =>
          formatCharacter(c, value)

        case d: FormatSpecifier.DecimalIntegral =>
          formatDecimalIntegral(d, value)

        case o: FormatSpecifier.OctalIntegral =>
          formatOctalIntegral(o, value)

        case h: FormatSpecifier.HexIntegral =>
          formatHexIntegral(h, value)

        case f: FormatSpecifier.FloatingPoint =>
          formatFloatingPoint(f, value)

        case dt: FormatSpecifier.DateTime =>
          formatDateTime(dt, value)
      }
    }

  // --- General Conversions (%s, %S, %b, %B, %h, %H) ---

  private def formatGeneral(spec: FormatSpecifier.General,
                            value: Any): String = {
    val raw = spec.conversion match {
      case FormatSpecifier.GeneralConversion.StringConversion(_) =>
        if (value == null) "null" else value.toString

      case FormatSpecifier.GeneralConversion.BooleanConversion(_) =>
        if (value == null) "false"
        else value match {
          case b: Boolean => b.toString
          case b: java.lang.Boolean => b.toString
          case _ => "true"
        }

      case FormatSpecifier.GeneralConversion.HashCodeConversion(_) =>
        if (value == null) "null"
        else java.lang.Integer.toHexString(value.hashCode())
    }

    val truncated = spec.precision.fold(raw)(p => if (p < raw.length) raw.substring(0, p) else raw)
    val cased = if (spec.conversion.isUpperCase) truncated.toUpperCase else truncated
    justify(cased, spec.width, spec.justification)
  }

  // --- Character Conversions (%c, %C) ---

  private def formatCharacter(spec: FormatSpecifier.Character,
                              value: Any): Either[RuntimeFormattingError, String] = {
    val codePointEither: Either[RuntimeFormattingError, Int] = value match {
      case c: Char => Right(c.toInt)
      case b: Byte => Right(b.toInt)
      case s: Short => Right(s.toInt)
      case i: Int => Right(i)
      case c: java.lang.Character => Right(c.charValue().toInt)
      case b: java.lang.Byte => Right(b.byteValue().toInt)
      case s: java.lang.Short => Right(s.shortValue().toInt)
      case i: java.lang.Integer => Right(i.intValue())
      case _ =>
        Left(RuntimeFormattingError.IncompatibleType(
          specifier = spec,
          actualType = value.getClass.getName,
          message = s"Conversion = c, Flags = "
        ))
    }

    codePointEither.flatMap { cp =>
      if (cp < 0 || cp > 0x10ffff) {
        Left(RuntimeFormattingError.IllegalCodePoint(spec, cp))
      } else {
        val raw = if (cp <= 0xffff) {
          cp.toChar.toString
        } else {
          new String(java.lang.Character.toChars(cp))
        }
        val cased = if (spec.isUpperCase) raw.toUpperCase else raw
        Right(justify(cased, spec.width, spec.justification))
      }
    }
  }

  // --- Decimal Integral (%d) ---

  private def formatDecimalIntegral(spec: FormatSpecifier.DecimalIntegral,
                                    value: Any): Either[RuntimeFormattingError, String] =
    extractIntegral(spec, value).map {
      case Left((l, isNegative)) =>
        val absDigits = if (l == Long.MinValue) "9223372036854775808" else Math.abs(l).toString
        val groupedDigits = if (spec.groupingSeparator) addGroupingSeparators(absDigits) else absDigits
        val (prefix, suffix) = resolveSign(spec.sign, isNegative)
        formatAlignedDecimal(prefix, groupedDigits, suffix, spec.alignment)

      case Right((bi, isNegative)) =>
        val absDigits = bi.abs.toString()
        val groupedDigits = if (spec.groupingSeparator) addGroupingSeparators(absDigits) else absDigits
        val (prefix, suffix) = resolveSign(spec.sign, isNegative)
        formatAlignedDecimal(prefix, groupedDigits, suffix, spec.alignment)
    }

  // --- Octal Integral (%o) ---

  private def formatOctalIntegral(spec: FormatSpecifier.OctalIntegral,
                                  value: Any): Either[RuntimeFormattingError, String] =
    extractIntegral(spec, value).map {
      case Left((l, _)) =>
        val octDigits = java.lang.Long.toOctalString(l)
        val formatted = if (spec.alternateForm && !octDigits.startsWith("0")) "0" + octDigits else octDigits
        formatAlignedDecimal("", formatted, "", spec.alignment)

      case Right((bi, _)) =>
        val octDigits = bi.toString(8)
        val formatted = if (spec.alternateForm && !octDigits.startsWith("0")) "0" + octDigits else octDigits
        formatAlignedDecimal("", formatted, "", spec.alignment)
    }

  // --- Hex Integral (%x, %X) ---

  private def formatHexIntegral(spec: FormatSpecifier.HexIntegral,
                                value: Any): Either[RuntimeFormattingError, String] =
    extractIntegral(spec, value).map {
      case Left((l, _)) =>
        val rawDigits = java.lang.Long.toHexString(l)
        val digits = if (spec.isUpperCase) rawDigits.toUpperCase else rawDigits.toLowerCase
        val prefix = if (spec.alternateForm) if (spec.isUpperCase) "0X" else "0x" else ""
        formatAlignedDecimal(prefix, digits, "", spec.alignment)

      case Right((bi, _)) =>
        val rawDigits = bi.toString(16)
        val digits = if (spec.isUpperCase) rawDigits.toUpperCase else rawDigits.toLowerCase
        val prefix = if (spec.alternateForm) if (spec.isUpperCase) "0X" else "0x" else ""
        formatAlignedDecimal(prefix, digits, "", spec.alignment)
    }

  // --- Floating-Point (%f, %e, %E, %g, %G, %a, %A) ---

  private def formatFloatingPoint(spec: FormatSpecifier.FloatingPoint,
                                  value: Any): Either[RuntimeFormattingError, String] = {
    val doubleEither: Either[RuntimeFormattingError, Double] = value match {
      case d: Double => Right(d)
      case f: Float => Right(f.toDouble)
      case bd: scala.math.BigDecimal => Right(bd.toDouble)
      case bd: java.math.BigDecimal => Right(bd.doubleValue())
      case _ =>
        Left(RuntimeFormattingError.IncompatibleType(
          specifier = spec,
          actualType = value.getClass.getName,
          message = s"Conversion = f, Flags = "
        ))
    }

    doubleEither.flatMap { d =>
      spec.conversion match {
        case FormatSpecifier.FloatConversion.HexadecimalFloat(_) =>
          Left(RuntimeFormattingError.IncompatibleType(
            specifier = spec,
            actualType = value.getClass.getName,
            message = "Hexadecimal floating point conversion (%a/%A) is not supported in Scala.js"
          ))

        case FormatSpecifier.FloatConversion.DecimalFloat =>
          Right(formatDecimalFloat(spec, d))

        case FormatSpecifier.FloatConversion.Scientific(isUpper) =>
          Right(formatScientific(spec, d, isUpper))

        case FormatSpecifier.FloatConversion.GeneralScientific(isUpper) =>
          Right(formatGeneralScientific(spec, d, isUpper))
      }
    }
  }

  private def formatDecimalFloat(spec: FormatSpecifier.FloatingPoint,
                                 d: Double): String =
    if (d.isNaN) {
      formatSpecialFloatingPoint("NaN", spec.alignment)
    } else if (d.isPosInfinity) {
      val prefix = spec.sign match {
        case FormatSpecifier.SignFormat.AlwaysSign => "+"
        case FormatSpecifier.SignFormat.SpaceForPositive => " "
        case _ => ""
      }
      formatSpecialFloatingPoint(prefix + "Infinity", spec.alignment)
    } else if (d.isNegInfinity) {
      val (prefix, suffix) = spec.sign match {
        case FormatSpecifier.SignFormat.EncloseNegative => ("(", ")")
        case _ => ("-", "")
      }
      formatSpecialFloatingPoint(prefix + "Infinity" + suffix, spec.alignment)
    } else {
      val isNegative = d < 0.0 || (d == 0.0 && 1.0 / d < 0.0)
      val absVal = Math.abs(d)
      val prec = spec.precision.fold(6)(identity)
      val raw = absVal.asInstanceOf[js.Dynamic].toFixed(prec).asInstanceOf[String]

      val dotIndex = raw.indexOf('.')
      val intPart = if (dotIndex >= 0) raw.substring(0, dotIndex) else raw
      val fracPart = if (dotIndex >= 0) raw.substring(dotIndex + 1) else ""
      val groupedInt = if (spec.groupingSeparator) addGroupingSeparators(intPart) else intPart

      val numStr = if (prec == 0) {
        if (spec.alternateForm) groupedInt + "." else groupedInt
      } else {
        s"$groupedInt.$fracPart"
      }

      val (prefix, suffix) = resolveSign(spec.sign, isNegative)
      formatAlignedDecimal(prefix, numStr, suffix, spec.alignment)
    }

  private def formatScientific(spec: FormatSpecifier.FloatingPoint,
                               d: Double,
                               isUpperCase: Boolean): String =
    if (d.isNaN) {
      formatSpecialFloatingPoint("NaN", spec.alignment)
    } else if (d.isPosInfinity) {
      val prefix = spec.sign match {
        case FormatSpecifier.SignFormat.AlwaysSign => "+"
        case FormatSpecifier.SignFormat.SpaceForPositive => " "
        case _ => ""
      }
      formatSpecialFloatingPoint(prefix + "Infinity", spec.alignment)
    } else if (d.isNegInfinity) {
      val (prefix, suffix) = spec.sign match {
        case FormatSpecifier.SignFormat.EncloseNegative => ("(", ")")
        case _ => ("-", "")
      }
      formatSpecialFloatingPoint(prefix + "Infinity" + suffix, spec.alignment)
    } else {
      val isNegative = d < 0.0 || (d == 0.0 && 1.0 / d < 0.0)
      val absVal = Math.abs(d)
      val prec = spec.precision.fold(6)(identity)
      val raw = absVal.asInstanceOf[js.Dynamic].toExponential(prec).asInstanceOf[String]
      val normalizedExp = formatExponential(raw, isUpperCase)

      val (prefix, suffix) = resolveSign(spec.sign, isNegative)
      formatAlignedDecimal(prefix, normalizedExp, suffix, spec.alignment)
    }

  private def formatGeneralScientific(spec: FormatSpecifier.FloatingPoint,
                                      d: Double,
                                      isUpperCase: Boolean): String =
    if (d.isNaN) {
      formatSpecialFloatingPoint("NaN", spec.alignment)
    } else if (d.isPosInfinity) {
      val prefix = spec.sign match {
        case FormatSpecifier.SignFormat.AlwaysSign => "+"
        case FormatSpecifier.SignFormat.SpaceForPositive => " "
        case _ => ""
      }
      formatSpecialFloatingPoint(prefix + "Infinity", spec.alignment)
    } else if (d.isNegInfinity) {
      val (prefix, suffix) = spec.sign match {
        case FormatSpecifier.SignFormat.EncloseNegative => ("(", ")")
        case _ => ("-", "")
      }
      formatSpecialFloatingPoint(prefix + "Infinity" + suffix, spec.alignment)
    } else {
      val isNegative = d < 0.0 || (d == 0.0 && 1.0 / d < 0.0)
      val absVal = Math.abs(d)
      val prec = spec.precision.fold(6)(p => if (p == 0) 1 else p)
      val raw = absVal.asInstanceOf[js.Dynamic].toPrecision(prec).asInstanceOf[String]
      val normalized = if (raw.contains('e') || raw.contains('E')) {
        formatExponential(raw, isUpperCase)
      } else {
        if (spec.groupingSeparator) {
          val dotIdx = raw.indexOf('.')
          val intPart = if (dotIdx >= 0) raw.substring(0, dotIdx) else raw
          val fracPart = if (dotIdx >= 0) raw.substring(dotIdx) else ""
          addGroupingSeparators(intPart) + fracPart
        } else {
          raw
        }
      }

      val (prefix, suffix) = resolveSign(spec.sign, isNegative)
      formatAlignedDecimal(prefix, normalized, suffix, spec.alignment)
    }

  private def formatSpecialFloatingPoint(str: String,
                                         alignment: FormatSpecifier.Alignment): String =
    alignment match {
      case FormatSpecifier.Alignment.Unspecified => str
      case l: FormatSpecifier.Alignment.Left =>
        if (str.length < l.width) str + (" " * (l.width - str.length)) else str
      case r: FormatSpecifier.Alignment.Right =>
        if (str.length < r.width) (" " * (r.width - str.length)) + str else str
      case z: FormatSpecifier.Alignment.ZeroPad =>
        if (str.length < z.width) (" " * (z.width - str.length)) + str else str
    }

  // --- Date & Time (%t, %T) ---

  private def formatDateTime(spec: FormatSpecifier.DateTime,
                             value: Any): Either[RuntimeFormattingError, String] = {
    val dateEither: Either[RuntimeFormattingError, js.Date] = value match {
      case l: Long =>
        Right(new js.Date(l.toDouble))
      case d: js.Date =>
        Right(new js.Date(d.getTime()))
      case d: java.util.Date =>
        Right(new js.Date(d.getTime().toDouble))
      case _ =>
        Left(RuntimeFormattingError.IncompatibleType(
          specifier = spec,
          actualType = value.getClass.getName,
          message = s"Conversion = t, Flags = "
        ))
    }

    dateEither.map { jsDate =>
      val raw = formatDateTimeSuffix(spec.suffix, jsDate)
      val cased = if (spec.isUpperCase) raw.toUpperCase else raw
      justify(cased, spec.width, spec.justification)
    }
  }

  private def formatDateTimeSuffix(suffix: FormatSpecifier.DateTimeSuffix,
                                   jsDate: js.Date): String = {
    val year = jsDate.getFullYear().toInt
    val monthIndex = jsDate.getMonth().toInt
    val dayOfMonth = jsDate.getDate().toInt
    val hours24 = jsDate.getHours().toInt
    val minutes = jsDate.getMinutes().toInt
    val seconds = jsDate.getSeconds().toInt
    val millis = jsDate.getMilliseconds().toInt
    val dayOfWeek = jsDate.getDay().toInt

    val hours12 = if (hours24 == 0) 12 else if (hours24 > 12) hours24 - 12 else hours24
    val ampm = if (hours24 < 12) "am" else "pm"

    suffix match {
      case FormatSpecifier.DateTimeSuffix.Hour24 =>
        pad2(hours24)

      case FormatSpecifier.DateTimeSuffix.Hour12 =>
        pad2(hours12)

      case FormatSpecifier.DateTimeSuffix.Hour24NoPad =>
        hours24.toString

      case FormatSpecifier.DateTimeSuffix.Hour12NoPad =>
        hours12.toString

      case FormatSpecifier.DateTimeSuffix.Minute =>
        pad2(minutes)

      case FormatSpecifier.DateTimeSuffix.Second =>
        pad2(seconds)

      case FormatSpecifier.DateTimeSuffix.Millisecond =>
        pad3(millis)

      case FormatSpecifier.DateTimeSuffix.Nanosecond =>
        pad3(millis) + "000000"

      case FormatSpecifier.DateTimeSuffix.AmPm =>
        ampm

      case FormatSpecifier.DateTimeSuffix.TimeZoneOffset =>
        val offsetMin = -jsDate.getTimezoneOffset().toInt
        val sign = if (offsetMin >= 0) "+" else "-"
        val absMin = Math.abs(offsetMin)
        s"$sign${pad2(absMin / 60)}${pad2(absMin % 60)}"

      case FormatSpecifier.DateTimeSuffix.TimeZoneName =>
        val offsetMin = -jsDate.getTimezoneOffset().toInt
        if (offsetMin == 0) "UTC"
        else {
          val sign = if (offsetMin >= 0) "+" else "-"
          val absMin = Math.abs(offsetMin)
          s"GMT$sign${pad2(absMin / 60)}:${pad2(absMin % 60)}"
        }

      case FormatSpecifier.DateTimeSuffix.EpochSeconds =>
        (jsDate.getTime() / 1000).toLong.toString

      case FormatSpecifier.DateTimeSuffix.EpochMillis =>
        jsDate.getTime().toLong.toString

      case FormatSpecifier.DateTimeSuffix.MonthNameFull =>
        MonthNamesFull(monthIndex)

      case FormatSpecifier.DateTimeSuffix.MonthNameAbbr | FormatSpecifier.DateTimeSuffix.MonthNameAbbrAlt =>
        MonthNamesAbbr(monthIndex)

      case FormatSpecifier.DateTimeSuffix.DayOfWeekFull =>
        DayNamesFull(dayOfWeek)

      case FormatSpecifier.DateTimeSuffix.DayOfWeekAbbr =>
        DayNamesAbbr(dayOfWeek)

      case FormatSpecifier.DateTimeSuffix.Century =>
        pad2(year / 100)

      case FormatSpecifier.DateTimeSuffix.Year4Digit =>
        pad4(year)

      case FormatSpecifier.DateTimeSuffix.Year2Digit =>
        pad2(Math.abs(year) % 100)

      case FormatSpecifier.DateTimeSuffix.DayOfYear =>
        val isLeap = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
        val monthDays = if (isLeap) LeapMonthOffsets else NormalMonthOffsets
        val doy = monthDays(monthIndex) + dayOfMonth
        pad3(doy)

      case FormatSpecifier.DateTimeSuffix.MonthNumber =>
        pad2(monthIndex + 1)

      case FormatSpecifier.DateTimeSuffix.DayOfMonthPadded =>
        pad2(dayOfMonth)

      case FormatSpecifier.DateTimeSuffix.DayOfMonth =>
        padSpace2(dayOfMonth)

      case FormatSpecifier.DateTimeSuffix.Time24Short =>
        s"${pad2(hours24)}:${pad2(minutes)}"

      case FormatSpecifier.DateTimeSuffix.Time24Long =>
        s"${pad2(hours24)}:${pad2(minutes)}:${pad2(seconds)}"

      case FormatSpecifier.DateTimeSuffix.Time12Long =>
        val upperAmPm = if (hours24 < 12) "AM" else "PM"
        s"${pad2(hours12)}:${pad2(minutes)}:${pad2(seconds)} $upperAmPm"

      case FormatSpecifier.DateTimeSuffix.DateSlash =>
        s"${pad2(monthIndex + 1)}/${pad2(dayOfMonth)}/${pad2(Math.abs(year) % 100)}"

      case FormatSpecifier.DateTimeSuffix.IsoDate =>
        s"${pad4(year)}-${pad2(monthIndex + 1)}-${pad2(dayOfMonth)}"

      case FormatSpecifier.DateTimeSuffix.FullDateTime =>
        val tz = {
          val offsetMin = -jsDate.getTimezoneOffset().toInt
          if (offsetMin == 0) "UTC"
          else {
            val sign = if (offsetMin >= 0) "+" else "-"
            val absMin = Math.abs(offsetMin)
            s"GMT$sign${pad2(absMin / 60)}:${pad2(absMin % 60)}"
          }
        }
        s"${DayNamesAbbr(dayOfWeek)} ${MonthNamesAbbr(monthIndex)} ${pad2(dayOfMonth)} ${pad2(hours24)}:${pad2(minutes)}:${pad2(seconds)} $tz ${pad4(year)}"
    }
  }

  // --- Helper & Utility Functions ---

  private def extractIntegral(spec: FormatSpecifier,
                              value: Any): Either[RuntimeFormattingError, Either[(Long, Boolean), (BigInt, Boolean)]] =
    value match {
      case b: Byte =>
        val l = b.toLong
        Right(Left((l, l < 0L)))
      case s: Short =>
        val l = s.toLong
        Right(Left((l, l < 0L)))
      case i: Int =>
        val l = i.toLong
        Right(Left((l, l < 0L)))
      case l: Long =>
        Right(Left((l, l < 0L)))
      case bi: scala.math.BigInt =>
        Right(Right((bi, bi < 0)))
      case bi: java.math.BigInteger =>
        val scalaBi = scala.math.BigInt(bi)
        Right(Right((scalaBi, scalaBi < 0)))
      case _ =>
        Left(RuntimeFormattingError.IncompatibleType(
          specifier = spec,
          actualType = value.getClass.getName,
          message = s"Conversion = d, Flags = "
        ))
    }

  private def resolveSign(sign: FormatSpecifier.SignFormat,
                          isNegative: Boolean): (String, String) =
    if (isNegative) {
      sign match {
        case FormatSpecifier.SignFormat.EncloseNegative => ("(", ")")
        case _ => ("-", "")
      }
    } else {
      sign match {
        case FormatSpecifier.SignFormat.AlwaysSign => ("+", "")
        case FormatSpecifier.SignFormat.SpaceForPositive => (" ", "")
        case _ => ("", "")
      }
    }

  private def formatAlignedDecimal(prefix: String,
                                   digits: String,
                                   suffix: String,
                                   alignment: FormatSpecifier.Alignment): String =
    alignment match {
      case FormatSpecifier.Alignment.Unspecified =>
        prefix + digits + suffix

      case l: FormatSpecifier.Alignment.Left =>
        val raw = prefix + digits + suffix
        if (raw.length < l.width) raw + (" " * (l.width - raw.length)) else raw

      case r: FormatSpecifier.Alignment.Right =>
        val raw = prefix + digits + suffix
        if (raw.length < r.width) (" " * (r.width - raw.length)) + raw else raw

      case z: FormatSpecifier.Alignment.ZeroPad =>
        val currentLen = prefix.length + digits.length + suffix.length
        if (currentLen < z.width) {
          prefix + ("0" * (z.width - currentLen)) + digits + suffix
        } else {
          prefix + digits + suffix
        }
    }

  private def addGroupingSeparators(digits: String): String = {
    val len = digits.length
    if (len <= 3) digits
    else {
      val remainder = len % 3
      val firstChunkSize = if (remainder == 0) 3 else remainder
      val sb = new java.lang.StringBuilder()
      sb.append(digits.substring(0, firstChunkSize))
      var i = firstChunkSize
      while (i < len) {
        sb.append(',')
        sb.append(digits.substring(i, i + 3))
        i += 3
      }
      sb.toString
    }
  }

  private def formatExponential(rawExp: String,
                                isUpperCase: Boolean): String = {
    val eIdx = {
      val idxLower = rawExp.indexOf('e')
      if (idxLower >= 0) idxLower else rawExp.indexOf('E')
    }
    if (eIdx < 0) rawExp
    else {
      val mantissa = rawExp.substring(0, eIdx)
      val expPart = rawExp.substring(eIdx + 1)
      val (signChar, digits) = if (expPart.startsWith("+") || expPart.startsWith("-")) {
        (expPart.charAt(0).toString, expPart.substring(1))
      } else {
        ("+", expPart)
      }
      val paddedDigits = if (digits.length < 2) "0" * (2 - digits.length) + digits else digits
      val expLetter = if (isUpperCase) "E" else "e"
      s"$mantissa$expLetter$signChar$paddedDigits"
    }
  }

  private def justify(str: String,
                      widthOpt: Option[Int],
                      justification: FormatSpecifier.Justification): String =
    widthOpt match {
      case Some(width) if width > str.length =>
        val padding = " " * (width - str.length)
        justification match {
          case FormatSpecifier.Justification.Left => str + padding
          case FormatSpecifier.Justification.Right => padding + str
        }
      case _ => str
    }

  private def pad2(n: Int): String =
    if (n >= 0 && n < 10) "0" + n
    else if (n < 0 && n > -10) "-0" + (-n)
    else n.toString

  private def padSpace2(n: Int): String =
    if (n >= 0 && n < 10) " " + n
    else n.toString

  private def pad3(n: Int): String =
    if (n >= 0 && n < 10) "00" + n
    else if (n >= 10 && n < 100) "0" + n
    else n.toString

  private def pad4(n: Int): String =
    if (n >= 0 && n < 10) "000" + n
    else if (n >= 10 && n < 100) "00" + n
    else if (n >= 100 && n < 1000) "0" + n
    else n.toString

  private val MonthNamesFull: Array[String] = Array(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )

  private val MonthNamesAbbr: Array[String] = Array(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )

  private val DayNamesFull: Array[String] = Array(
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
  )

  private val DayNamesAbbr: Array[String] = Array(
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
  )

  private val NormalMonthOffsets: Array[Int] = Array(
    0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
  )

  private val LeapMonthOffsets: Array[Int] = Array(
    0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
  )
}
