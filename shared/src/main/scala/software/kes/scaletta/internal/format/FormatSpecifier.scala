package software.kes.scaletta.internal.format

sealed trait FormatSpecifier

object FormatSpecifier {
  // --- Alignment & Justification ---

  sealed trait Alignment

  object Alignment {
    case object Unspecified extends Alignment

    final class Left private(val width: Int) extends Alignment {
      override def equals(other: Any): Boolean = other match {
        case that: Left =>
          width == that.width
        case _ => false
      }

      override def hashCode(): Int =
        width.hashCode()

      override def toString: String = s"Left($width)"
    }

    object Left {
      def apply(width: Int): Left = new Left(clamp(width))

      def unapply(arg: Left): Option[Int] = Some(arg.width)
    }

    final class Right private(val width: Int) extends Alignment {
      override def equals(other: Any): Boolean = other match {
        case that: Right =>
          width == that.width
        case _ => false
      }

      override def hashCode(): Int =
        width.hashCode()

      override def toString: String = s"Right($width)"
    }

    object Right {
      def apply(width: Int): Right = new Right(clamp(width))

      def unapply(arg: Right): Option[Int] = Some(arg.width)
    }

    final class ZeroPad private(val width: Int) extends Alignment {
      override def equals(other: Any): Boolean = other match {
        case that: ZeroPad =>
          width == that.width
        case _ => false
      }

      override def hashCode(): Int =
        width.hashCode()

      override def toString: String = s"ZeroPad($width)"
    }

    object ZeroPad {
      def apply(width: Int): ZeroPad = new ZeroPad(clamp(width))

      def unapply(arg: ZeroPad): Option[Int] = Some(arg.width)
    }

    def left(width: Int): Alignment = Left(width)

    def right(width: Int): Alignment = Right(width)

    def zeroPad(width: Int): Alignment = ZeroPad(width)
  }

  sealed trait Justification

  object Justification {
    case object Right extends Justification

    case object Left extends Justification
  }

  // --- Numeric Sign Handling ---

  sealed trait SignFormat

  object SignFormat {
    case object Default extends SignFormat

    case object AlwaysSign extends SignFormat

    case object SpaceForPositive extends SignFormat

    case object EncloseNegative extends SignFormat
  }

  // --- General Conversions (s, S, b, B, h, H) ---

  sealed trait GeneralConversion {
    def isUpperCase: Boolean
  }

  object GeneralConversion {
    final case class StringConversion(isUpperCase: Boolean) extends GeneralConversion

    final case class BooleanConversion(isUpperCase: Boolean) extends GeneralConversion

    final case class HashCodeConversion(isUpperCase: Boolean) extends GeneralConversion
  }

  final class General private(val conversion: GeneralConversion,
                              val width: Option[Int],
                              val precision: Option[Int],
                              val justification: Justification) extends FormatSpecifier {

    override def equals(other: Any): Boolean = other match {
      case that: General =>
        conversion == that.conversion &&
          width == that.width &&
          precision == that.precision &&
          justification == that.justification
      case _ => false
    }

    override def hashCode(): Int = {
      var result = conversion.hashCode()
      result = 31 * result + width.hashCode()
      result = 31 * result + precision.hashCode()
      result = 31 * result + justification.hashCode()
      result
    }

    override def toString: String = s"General($conversion, $width, $precision, $justification)"
  }

  object General {
    def apply(conversion: GeneralConversion,
              width: Option[Int] = None,
              precision: Option[Int] = None,
              justification: Justification = Justification.Right): General =
      new General(conversion, clamp(width), clamp(precision), justification)

    def unapply(arg: General): Option[(GeneralConversion, Option[Int], Option[Int], Justification)] =
      Some((arg.conversion, arg.width, arg.precision, arg.justification))
  }

  // --- Character Conversions (c, C) ---

  final class Character private(val isUpperCase: Boolean,
                                val width: Option[Int],
                                val justification: Justification) extends FormatSpecifier {
    override def equals(other: Any): Boolean = other match {
      case that: Character =>
        isUpperCase == that.isUpperCase &&
          width == that.width &&
          justification == that.justification
      case _ => false
    }

    override def hashCode(): Int = {
      var result = isUpperCase.hashCode()
      result = 31 * result + width.hashCode()
      result = 31 * result + justification.hashCode()
      result
    }

    override def toString: String = s"Character($isUpperCase, $width, $justification)"
  }

  object Character {
    def apply(isUpperCase: Boolean,
              width: Option[Int] = None,
              justification: Justification = Justification.Right): Character =
      new Character(isUpperCase, clamp(width), justification)

    def unapply(arg: Character): Option[(Boolean, Option[Int], Justification)] =
      Some((arg.isUpperCase, arg.width, arg.justification))
  }

  // --- Decimal Integral Conversion (d) ---

  final case class DecimalIntegral(alignment: Alignment = Alignment.Unspecified,
                                   sign: SignFormat = SignFormat.Default,
                                   groupingSeparator: Boolean = false) extends FormatSpecifier

  // --- Octal Integral Conversion (o) ---

  final case class OctalIntegral(alignment: Alignment = Alignment.Unspecified,
                                 alternateForm: Boolean = false) extends FormatSpecifier

  // --- Hexadecimal Integral Conversion (x, X) ---

  final case class HexIntegral(isUpperCase: Boolean,
                               alignment: Alignment = Alignment.Unspecified,
                               alternateForm: Boolean = false) extends FormatSpecifier

  // --- Floating-Point Conversions (f, e, E, g, G, a, A) ---

  sealed trait FloatConversion

  object FloatConversion {
    case object DecimalFloat extends FloatConversion

    final case class Scientific(isUpperCase: Boolean) extends FloatConversion

    final case class GeneralScientific(isUpperCase: Boolean) extends FloatConversion

    final case class HexadecimalFloat(isUpperCase: Boolean) extends FloatConversion
  }

  final class FloatingPoint private(val conversion: FloatConversion,
                                    val alignment: Alignment,
                                    val sign: SignFormat,
                                    val alternateForm: Boolean,
                                    val groupingSeparator: Boolean,
                                    val precision: Option[Int]) extends FormatSpecifier {
    override def equals(other: Any): Boolean = other match {
      case that: FloatingPoint =>
        conversion == that.conversion &&
          alignment == that.alignment &&
          sign == that.sign &&
          alternateForm == that.alternateForm &&
          groupingSeparator == that.groupingSeparator &&
          precision == that.precision
      case _ => false
    }

    override def hashCode(): Int = {
      var result = conversion.hashCode()
      result = 31 * result + alignment.hashCode()
      result = 31 * result + sign.hashCode()
      result = 31 * result + alternateForm.hashCode()
      result = 31 * result + groupingSeparator.hashCode()
      result = 31 * result + precision.hashCode()
      result
    }

    override def toString: String = s"FloatingPoint($conversion, $alignment, $sign, $alternateForm, $groupingSeparator, $precision)"
  }

  object FloatingPoint {
    def apply(conversion: FloatConversion,
              alignment: Alignment = Alignment.Unspecified,
              sign: SignFormat = SignFormat.Default,
              alternateForm: Boolean = false,
              groupingSeparator: Boolean = false,
              precision: Option[Int] = None): FloatingPoint =
      new FloatingPoint(conversion, alignment, sign, alternateForm, groupingSeparator, clamp(precision))

    def unapply(arg: FloatingPoint): Option[(FloatConversion, Alignment, SignFormat, Boolean, Boolean, Option[Int])] =
      Some((arg.conversion, arg.alignment, arg.sign, arg.alternateForm, arg.groupingSeparator, arg.precision))
  }

  // --- Date / Time Suffixes (t, T) ---

  sealed trait DateTimeSuffix {
    def character: Char
  }

  object DateTimeSuffix {
    case object Hour24 extends DateTimeSuffix {
      def character: Char = 'H'
    }

    case object Hour12 extends DateTimeSuffix {
      def character: Char = 'I'
    }

    case object Hour24NoPad extends DateTimeSuffix {
      def character: Char = 'k'
    }

    case object Hour12NoPad extends DateTimeSuffix {
      def character: Char = 'l'
    }

    case object Minute extends DateTimeSuffix {
      def character: Char = 'M'
    }

    case object Second extends DateTimeSuffix {
      def character: Char = 'S'
    }

    case object Millisecond extends DateTimeSuffix {
      def character: Char = 'L'
    }

    case object Nanosecond extends DateTimeSuffix {
      def character: Char = 'N'
    }

    case object AmPm extends DateTimeSuffix {
      def character: Char = 'p'
    }

    case object TimeZoneOffset extends DateTimeSuffix {
      def character: Char = 'z'
    }

    case object TimeZoneName extends DateTimeSuffix {
      def character: Char = 'Z'
    }

    case object EpochSeconds extends DateTimeSuffix {
      def character: Char = 's'
    }

    case object EpochMillis extends DateTimeSuffix {
      def character: Char = 'Q'
    }

    case object MonthNameFull extends DateTimeSuffix {
      def character: Char = 'B'
    }

    case object MonthNameAbbr extends DateTimeSuffix {
      def character: Char = 'b'
    }

    case object MonthNameAbbrAlt extends DateTimeSuffix {
      def character: Char = 'h'
    }

    case object DayOfWeekFull extends DateTimeSuffix {
      def character: Char = 'A'
    }

    case object DayOfWeekAbbr extends DateTimeSuffix {
      def character: Char = 'a'
    }

    case object Century extends DateTimeSuffix {
      def character: Char = 'C'
    }

    case object Year4Digit extends DateTimeSuffix {
      def character: Char = 'Y'
    }

    case object Year2Digit extends DateTimeSuffix {
      def character: Char = 'y'
    }

    case object DayOfYear extends DateTimeSuffix {
      def character: Char = 'j'
    }

    case object MonthNumber extends DateTimeSuffix {
      def character: Char = 'm'
    }

    case object DayOfMonthPadded extends DateTimeSuffix {
      def character: Char = 'd'
    }

    case object DayOfMonth extends DateTimeSuffix {
      def character: Char = 'e'
    }

    case object Time24Short extends DateTimeSuffix {
      def character: Char = 'R'
    }

    case object Time24Long extends DateTimeSuffix {
      def character: Char = 'T'
    }

    case object Time12Long extends DateTimeSuffix {
      def character: Char = 'r'
    }

    case object DateSlash extends DateTimeSuffix {
      def character: Char = 'D'
    }

    case object IsoDate extends DateTimeSuffix {
      def character: Char = 'F'
    }

    case object FullDateTime extends DateTimeSuffix {
      def character: Char = 'c'
    }

    def fromChar(c: Char): Option[DateTimeSuffix] =
      c match {
        case 'H' => Some(Hour24)
        case 'I' => Some(Hour12)
        case 'k' => Some(Hour24NoPad)
        case 'l' => Some(Hour12NoPad)
        case 'M' => Some(Minute)
        case 'S' => Some(Second)
        case 'L' => Some(Millisecond)
        case 'N' => Some(Nanosecond)
        case 'p' => Some(AmPm)
        case 'z' => Some(TimeZoneOffset)
        case 'Z' => Some(TimeZoneName)
        case 's' => Some(EpochSeconds)
        case 'Q' => Some(EpochMillis)
        case 'B' => Some(MonthNameFull)
        case 'b' => Some(MonthNameAbbr)
        case 'h' => Some(MonthNameAbbrAlt)
        case 'A' => Some(DayOfWeekFull)
        case 'a' => Some(DayOfWeekAbbr)
        case 'C' => Some(Century)
        case 'Y' => Some(Year4Digit)
        case 'y' => Some(Year2Digit)
        case 'j' => Some(DayOfYear)
        case 'm' => Some(MonthNumber)
        case 'd' => Some(DayOfMonthPadded)
        case 'e' => Some(DayOfMonth)
        case 'R' => Some(Time24Short)
        case 'T' => Some(Time24Long)
        case 'r' => Some(Time12Long)
        case 'D' => Some(DateSlash)
        case 'F' => Some(IsoDate)
        case 'c' => Some(FullDateTime)
        case _ => None
      }
  }

  final class DateTime private(val suffix: DateTimeSuffix,
                               val isUpperCase: Boolean,
                               val width: Option[Int],
                               val justification: Justification) extends FormatSpecifier {
    override def equals(other: Any): Boolean = other match {
      case that: DateTime =>
        suffix == that.suffix &&
          isUpperCase == that.isUpperCase &&
          width == that.width &&
          justification == that.justification
      case _ => false
    }

    override def hashCode(): Int = {
      var result = suffix.hashCode()
      result = 31 * result + isUpperCase.hashCode()
      result = 31 * result + width.hashCode()
      result = 31 * result + justification.hashCode()
      result
    }

    override def toString: String = s"DateTime($suffix, $isUpperCase, $width, $justification)"
  }

  object DateTime {
    def apply(suffix: DateTimeSuffix,
              isUpperCase: Boolean,
              width: Option[Int] = None,
              justification: Justification = Justification.Right): DateTime =
      new DateTime(suffix, isUpperCase, clamp(width), justification)

    def unapply(arg: DateTime): Option[(DateTimeSuffix, Boolean, Option[Int], Justification)] =
      Some((arg.suffix, arg.isUpperCase, arg.width, arg.justification))
  }

  private def clamp(value: Int): Int = Math.max(0, value)

  private def clamp(value: Option[Int]): Option[Int] =
    value match {
      case Some(v) => Some(Math.max(0, v))
      case None => None
    }
}
