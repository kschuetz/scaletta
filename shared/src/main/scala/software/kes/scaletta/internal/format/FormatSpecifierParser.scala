package software.kes.scaletta.internal.format

import software.kes.scaletta.internal.format.FormatSpecifier._
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}

object FormatSpecifierParser {

  sealed trait Result

  object Result {
    case class Success(specifier: Option[Pos[FormatSpecifier]],
                       suffix: Pos[String]) extends Result

    case class Error() extends Result
  }

  def parse(input: String,
            followsVariable: Boolean): Result = {
    if (followsVariable && startsWithSpecifier(input)) {
      parseSpecifier(input) match {
        case Some((specifier, consumedLength)) =>
          val specPos = Pos(specifier, CharIndex(0), CharIndex(consumedLength - 1))
          val remainder = input.substring(consumedLength)
          unescape(remainder) match {
            case Some(unescaped) =>
              val suffixPos = if (consumedLength >= input.length) {
                Pos(unescaped, CharIndex(input.length), CharIndex(input.length))
              } else {
                Pos(unescaped, CharIndex(consumedLength), CharIndex(input.length - 1))
              }
              Result.Success(Some(specPos), suffixPos)
            case None =>
              Result.Error()
          }
        case None =>
          Result.Error()
      }
    } else {
      unescape(input) match {
        case Some(unescaped) =>
          val suffixPos = if (input.isEmpty) {
            Pos(unescaped, CharIndex(0), CharIndex(0))
          } else {
            Pos(unescaped, CharIndex(0), CharIndex(input.length - 1))
          }
          Result.Success(None, suffixPos)
        case None =>
          Result.Error()
      }
    }
  }

  private def startsWithSpecifier(input: String): Boolean = {
    if (input.isEmpty || input.charAt(0) != '%') {
      false
    } else if (input.length >= 2 && (input.charAt(1) == '%' || input.charAt(1) == 'n')) {
      false
    } else {
      true
    }
  }

  private def parseSpecifier(input: String): Option[(FormatSpecifier, Int)] = {
    var i = 1
    val len = input.length

    var hasMinus = false
    var hasHash = false
    var hasPlus = false
    var hasSpace = false
    var hasZero = false
    var hasComma = false
    var hasParen = false

    var scanningFlags = true
    while (i < len && scanningFlags) {
      input.charAt(i) match {
        case '-' =>
          hasMinus = true
          i += 1
        case '#' =>
          hasHash = true
          i += 1
        case '+' =>
          hasPlus = true
          i += 1
        case ' ' =>
          hasSpace = true
          i += 1
        case '0' =>
          hasZero = true
          i += 1
        case ',' =>
          hasComma = true
          i += 1
        case '(' =>
          hasParen = true
          i += 1
        case _ =>
          scanningFlags = false
      }
    }

    if (hasMinus && hasZero) {
      None
    } else {
      var signCount = 0
      if (hasPlus) {
        signCount += 1
      }
      if (hasSpace) {
        signCount += 1
      }
      if (hasParen) {
        signCount += 1
      }

      if (signCount > 1) {
        None
      } else {
        var widthOpt: Option[Int] = None
        if (i < len && input.charAt(i).isDigit) {
          parseNumber(input, i) match {
            case Some((widthVal, nextIdx)) =>
              widthOpt = Some(widthVal)
              i = nextIdx
            case None =>
              return None
          }
        }

        var precisionOpt: Option[Int] = None
        if (i < len && input.charAt(i) == '.') {
          i += 1
          if (i >= len || !input.charAt(i).isDigit) {
            return None
          }
          parseNumber(input, i) match {
            case Some((precVal, nextIdx)) =>
              precisionOpt = Some(precVal)
              i = nextIdx
            case None =>
              return None
          }
        }

        if (i >= len) {
          None
        } else {
          val conv = input.charAt(i)
          i += 1

          conv match {
            case 's' | 'S' =>
              if (hasHash || hasPlus || hasSpace || hasZero || hasComma || hasParen) {
                None
              } else {
                val just = if (hasMinus) Justification.Left else Justification.Right
                Some((General(GeneralConversion.StringConversion(conv == 'S'), widthOpt, precisionOpt, just), i))
              }

            case 'b' | 'B' =>
              if (hasHash || hasPlus || hasSpace || hasZero || hasComma || hasParen) {
                None
              } else {
                val just = if (hasMinus) Justification.Left else Justification.Right
                Some((General(GeneralConversion.BooleanConversion(conv == 'B'), widthOpt, precisionOpt, just), i))
              }

            case 'h' | 'H' =>
              if (hasHash || hasPlus || hasSpace || hasZero || hasComma || hasParen) {
                None
              } else {
                val just = if (hasMinus) Justification.Left else Justification.Right
                Some((General(GeneralConversion.HashCodeConversion(conv == 'H'), widthOpt, precisionOpt, just), i))
              }

            case 'c' | 'C' =>
              if (hasHash || hasPlus || hasSpace || hasZero || hasComma || hasParen || precisionOpt.isDefined) {
                None
              } else {
                val just = if (hasMinus) Justification.Left else Justification.Right
                Some((Character(conv == 'C', widthOpt, just), i))
              }

            case 'd' =>
              if (hasHash || precisionOpt.isDefined) {
                None
              } else {
                val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
                val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
                Some((DecimalIntegral(align, sign, hasComma), i))
              }

            case 'o' =>
              if (hasPlus || hasSpace || hasParen || hasComma || precisionOpt.isDefined) {
                None
              } else {
                val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
                Some((OctalIntegral(align, hasHash), i))
              }

            case 'x' | 'X' =>
              if (hasPlus || hasSpace || hasParen || hasComma || precisionOpt.isDefined) {
                None
              } else {
                val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
                Some((HexIntegral(conv == 'X', align, hasHash), i))
              }

            case 'f' =>
              val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
              val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
              Some((FloatingPoint(FloatConversion.DecimalFloat, align, sign, hasHash, hasComma, precisionOpt), i))

            case 'e' | 'E' =>
              val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
              val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
              Some((FloatingPoint(FloatConversion.Scientific(conv == 'E'), align, sign, hasHash, hasComma, precisionOpt), i))

            case 'g' | 'G' =>
              val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
              val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
              Some((FloatingPoint(FloatConversion.GeneralScientific(conv == 'G'), align, sign, hasHash, hasComma, precisionOpt), i))

            case 'a' | 'A' =>
              if (hasComma) {
                None
              } else {
                val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
                val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
                Some((FloatingPoint(FloatConversion.HexadecimalFloat(conv == 'A'), align, sign, hasHash, false, precisionOpt), i))
              }

            case 't' | 'T' =>
              if (hasHash || hasPlus || hasSpace || hasZero || hasComma || hasParen || precisionOpt.isDefined) {
                None
              } else if (i >= len) {
                None
              } else {
                val suffixChar = input.charAt(i)
                i += 1
                DateTimeSuffix.fromChar(suffixChar) match {
                  case Some(suffix) =>
                    val just = if (hasMinus) Justification.Left else Justification.Right
                    Some((DateTime(suffix, conv == 'T', widthOpt, just), i))
                  case None =>
                    None
                }
              }

            case _ =>
              None
          }
        }
      }
    }
  }

  private def parseNumber(input: String,
                          startIndex: Int): Option[(Int, Int)] = {
    var idx = startIndex
    var acc: Long = 0L
    val len = input.length
    while (idx < len && input.charAt(idx).isDigit) {
      acc = acc * 10L + (input.charAt(idx) - '0')
      if (acc > Int.MaxValue) {
        return None
      }
      idx += 1
    }
    if (idx == startIndex) {
      None
    } else {
      Some((acc.toInt, idx))
    }
  }

  private def unescape(input: String): Option[String] = {
    val sb = new java.lang.StringBuilder()
    var i = 0
    val len = input.length
    while (i < len) {
      val ch = input.charAt(i)
      if (ch == '%') {
        if (i + 1 < len) {
          val next = input.charAt(i + 1)
          if (next == '%') {
            sb.append('%')
            i += 2
          } else if (next == 'n') {
            sb.append(System.lineSeparator())
            i += 2
          } else {
            return None
          }
        } else {
          return None
        }
      } else {
        sb.append(ch)
        i += 1
      }
    }
    Some(sb.toString)
  }
}
