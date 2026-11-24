package software.kes.scaletta.internal.format

import software.kes.scaletta.internal.format.FormatSpecifier._
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}

object FormatSpecifierParser {

  sealed trait Result

  object Result {
    final case class Success(specifier: Option[Pos[FormatSpecifier]],
                             suffix: Pos[String]) extends Result

    final case class Failure(error: Pos[FormatSpecifierError]) extends Result
  }

  def parse(input: String,
            followsVariable: Boolean): Result = {
    if (followsVariable && startsWithSpecifier(input)) {
      parseSpecifier(input) match {
        case Right((specifier, consumedLength)) =>
          val specPos = Pos(specifier, CharIndex(0), CharIndex(consumedLength - 1))
          val remainder = input.substring(consumedLength)
          unescape(remainder, consumedLength) match {
            case Right(unescaped) =>
              val suffixPos = if (consumedLength >= input.length) {
                Pos(unescaped, CharIndex(input.length), CharIndex(input.length))
              } else {
                Pos(unescaped, CharIndex(consumedLength), CharIndex(input.length - 1))
              }
              Result.Success(Some(specPos), suffixPos)
            case Left(err) =>
              Result.Failure(err)
          }
        case Left(err) =>
          Result.Failure(err)
      }
    } else {
      unescape(input, 0) match {
        case Right(unescaped) =>
          val suffixPos = if (input.isEmpty) {
            Pos(unescaped, CharIndex(0), CharIndex(0))
          } else {
            Pos(unescaped, CharIndex(0), CharIndex(input.length - 1))
          }
          Result.Success(None, suffixPos)
        case Left(err) =>
          Result.Failure(err)
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

  private def parseSpecifier(input: String): Either[Pos[FormatSpecifierError], (FormatSpecifier, Int)] = {
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

    var widthOpt: Option[Int] = None
    if (i < len && input.charAt(i).isDigit) {
      parseNumber(input, i) match {
        case Right((widthVal, nextIdx)) =>
          widthOpt = Some(widthVal)
          i = nextIdx
        case Left(true) =>
          var endIdx = i
          while (endIdx < len && input.charAt(endIdx).isDigit) {
            endIdx += 1
          }
          return Left(Pos(FormatSpecifierError.WidthOverflow, CharIndex(0), CharIndex(endIdx - 1)))
        case Left(false) =>
      }
    }

    var precisionOpt: Option[Int] = None
    if (i < len && input.charAt(i) == '.') {
      val dotIdx = i
      i += 1
      if (i >= len || !input.charAt(i).isDigit) {
        return Left(Pos(FormatSpecifierError.MissingPrecision, CharIndex(0), CharIndex(dotIdx)))
      }
      parseNumber(input, i) match {
        case Right((precVal, nextIdx)) =>
          precisionOpt = Some(precVal)
          i = nextIdx
        case Left(true) =>
          var endIdx = i
          while (endIdx < len && input.charAt(endIdx).isDigit) {
            endIdx += 1
          }
          return Left(Pos(FormatSpecifierError.PrecisionOverflow, CharIndex(0), CharIndex(endIdx - 1)))
        case Left(false) =>
          return Left(Pos(FormatSpecifierError.MissingPrecision, CharIndex(0), CharIndex(dotIdx)))
      }
    }

    if (i >= len) {
      return Left(Pos(FormatSpecifierError.MissingConversion, CharIndex(0), CharIndex(i - 1)))
    }

    val conv = input.charAt(i)
    val convIdx = i
    i += 1

    if (conv == 't' || conv == 'T') {
      if (i >= len) {
        return Left(Pos(FormatSpecifierError.MissingDateTimeSuffix, CharIndex(0), CharIndex(convIdx)))
      }
      val suffixChar = input.charAt(i)
      val suffixIdx = i
      i += 1
      DateTimeSuffix.fromChar(suffixChar) match {
        case None =>
          return Left(Pos(FormatSpecifierError.UnknownDateTimeSuffix(suffixChar), CharIndex(0), CharIndex(suffixIdx)))
        case Some(dtSuffix) =>
          if (hasMinus && hasZero) {
            return Left(Pos(FormatSpecifierError.ConflictingFlags('-', '0'), CharIndex(0), CharIndex(i - 1)))
          }
          var signFlags = Vector.empty[Char]
          if (hasPlus) signFlags :+= '+'
          if (hasSpace) signFlags :+= ' '
          if (hasParen) signFlags :+= '('
          if (signFlags.length > 1) {
            return Left(Pos(FormatSpecifierError.ConflictingSignFlags(signFlags), CharIndex(0), CharIndex(i - 1)))
          }
          if (hasHash) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('#', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasPlus) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('+', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasSpace) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(' ', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasZero) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('0', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasParen) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('(', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (precisionOpt.isDefined) {
            Left(Pos(FormatSpecifierError.PrecisionNotPermitted(conv), CharIndex(0), CharIndex(i - 1)))
          } else {
            val just = if (hasMinus) Justification.Left else Justification.Right
            Right((DateTime(dtSuffix, conv == 'T', widthOpt, just), i))
          }
      }
    } else {
      if (hasMinus && hasZero) {
        return Left(Pos(FormatSpecifierError.ConflictingFlags('-', '0'), CharIndex(0), CharIndex(i - 1)))
      }
      var signFlags = Vector.empty[Char]
      if (hasPlus) signFlags :+= '+'
      if (hasSpace) signFlags :+= ' '
      if (hasParen) signFlags :+= '('
      if (signFlags.length > 1) {
        return Left(Pos(FormatSpecifierError.ConflictingSignFlags(signFlags), CharIndex(0), CharIndex(i - 1)))
      }

      conv match {
        case 's' | 'S' =>
          if (hasHash) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('#', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasPlus) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('+', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasSpace) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(' ', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasZero) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('0', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasParen) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('(', conv), CharIndex(0), CharIndex(i - 1)))
          } else {
            val just = if (hasMinus) Justification.Left else Justification.Right
            Right((General(GeneralConversion.StringConversion(conv == 'S'), widthOpt, precisionOpt, just), i))
          }

        case 'b' | 'B' =>
          if (hasHash) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('#', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasPlus) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('+', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasSpace) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(' ', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasZero) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('0', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasParen) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('(', conv), CharIndex(0), CharIndex(i - 1)))
          } else {
            val just = if (hasMinus) Justification.Left else Justification.Right
            Right((General(GeneralConversion.BooleanConversion(conv == 'B'), widthOpt, precisionOpt, just), i))
          }

        case 'h' | 'H' =>
          if (hasHash) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('#', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasPlus) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('+', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasSpace) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(' ', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasZero) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('0', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasParen) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('(', conv), CharIndex(0), CharIndex(i - 1)))
          } else {
            val just = if (hasMinus) Justification.Left else Justification.Right
            Right((General(GeneralConversion.HashCodeConversion(conv == 'H'), widthOpt, precisionOpt, just), i))
          }

        case 'c' | 'C' =>
          if (hasHash) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('#', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasPlus) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('+', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasSpace) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(' ', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasZero) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('0', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasParen) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('(', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (precisionOpt.isDefined) {
            Left(Pos(FormatSpecifierError.PrecisionNotPermitted(conv), CharIndex(0), CharIndex(i - 1)))
          } else {
            val just = if (hasMinus) Justification.Left else Justification.Right
            Right((Character(conv == 'C', widthOpt, just), i))
          }

        case 'd' =>
          if (hasHash) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('#', 'd'), CharIndex(0), CharIndex(i - 1)))
          } else if (precisionOpt.isDefined) {
            Left(Pos(FormatSpecifierError.PrecisionNotPermitted('d'), CharIndex(0), CharIndex(i - 1)))
          } else {
            val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
            val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
            Right((DecimalIntegral(align, sign, hasComma), i))
          }

        case 'o' =>
          if (hasPlus) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('+', 'o'), CharIndex(0), CharIndex(i - 1)))
          } else if (hasSpace) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(' ', 'o'), CharIndex(0), CharIndex(i - 1)))
          } else if (hasParen) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('(', 'o'), CharIndex(0), CharIndex(i - 1)))
          } else if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', 'o'), CharIndex(0), CharIndex(i - 1)))
          } else if (precisionOpt.isDefined) {
            Left(Pos(FormatSpecifierError.PrecisionNotPermitted('o'), CharIndex(0), CharIndex(i - 1)))
          } else {
            val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
            Right((OctalIntegral(align, hasHash), i))
          }

        case 'x' | 'X' =>
          if (hasPlus) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('+', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasSpace) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(' ', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasParen) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag('(', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', conv), CharIndex(0), CharIndex(i - 1)))
          } else if (precisionOpt.isDefined) {
            Left(Pos(FormatSpecifierError.PrecisionNotPermitted(conv), CharIndex(0), CharIndex(i - 1)))
          } else {
            val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
            Right((HexIntegral(conv == 'X', align, hasHash), i))
          }

        case 'f' =>
          val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
          val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
          Right((FloatingPoint(FloatConversion.DecimalFloat, align, sign, hasHash, hasComma, precisionOpt), i))

        case 'e' | 'E' =>
          val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
          val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
          Right((FloatingPoint(FloatConversion.Scientific(conv == 'E'), align, sign, hasHash, hasComma, precisionOpt), i))

        case 'g' | 'G' =>
          val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
          val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
          Right((FloatingPoint(FloatConversion.GeneralScientific(conv == 'G'), align, sign, hasHash, hasComma, precisionOpt), i))

        case 'a' | 'A' =>
          if (hasComma) {
            Left(Pos(FormatSpecifierError.IncompatibleFlag(',', conv), CharIndex(0), CharIndex(i - 1)))
          } else {
            val sign = if (hasPlus) SignFormat.AlwaysSign else if (hasSpace) SignFormat.SpaceForPositive else if (hasParen) SignFormat.EncloseNegative else SignFormat.Default
            val align = if (hasMinus) Alignment.Left(widthOpt.getOrElse(0)) else if (hasZero) Alignment.ZeroPad(widthOpt.getOrElse(0)) else widthOpt.fold[Alignment](Alignment.Unspecified)(Alignment.Right(_))
            Right((FloatingPoint(FloatConversion.HexadecimalFloat(conv == 'A'), align, sign, hasHash, false, precisionOpt), i))
          }

        case _ =>
          Left(Pos(FormatSpecifierError.UnknownConversion(conv), CharIndex(0), CharIndex(convIdx)))
      }
    }
  }

  private def parseNumber(input: String,
                          startIndex: Int): Either[Boolean, (Int, Int)] = {
    var idx = startIndex
    var acc: Long = 0L
    val len = input.length
    while (idx < len && input.charAt(idx).isDigit) {
      acc = acc * 10L + (input.charAt(idx) - '0')
      if (acc > Int.MaxValue) {
        return Left(true)
      }
      idx += 1
    }
    if (idx == startIndex) {
      Left(false)
    } else {
      Right((acc.toInt, idx))
    }
  }

  private def unescape(input: String,
                       offset: Int): Either[Pos[FormatSpecifierError], String] = {
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
            return Left(Pos(FormatSpecifierError.InvalidEscape(next), CharIndex(offset + i), CharIndex(offset + i + 1)))
          }
        } else {
          return Left(Pos(FormatSpecifierError.LonePercent, CharIndex(offset + i), CharIndex(offset + i)))
        }
      } else {
        sb.append(ch)
        i += 1
      }
    }
    Right(sb.toString)
  }
}
