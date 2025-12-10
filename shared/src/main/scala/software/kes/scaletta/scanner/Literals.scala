package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.CharacterClass.{isDigit, isLetter}
import software.kes.scaletta.scanner.ScannerError._
import software.kes.scaletta.scanner.Token.{IntLiteral, LongLiteral, MultiLineString, StringLiteral}
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object Literals {

  /**
   * Assumes the opening ' has already been consumed
   */
  def charLiteral(reader: CharReader): Pos[Either[ScannerError, Token]] = {
    val begin = reader.prevIndex

    def inChar: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          if (ch == '\\') escapeSequence
          else if (ch == '\'') Pos(Left(EmptyCharacterLiteral), begin, reader.prevIndex)
          else endQuote(ch)
        case None => Pos(Left(UnclosedCharacterLiteral), begin, reader.prevIndex)
      }

    def escapeSequence: Pos[Either[ScannerError, Token]] =
      EscapeSequence.scan(reader) match {
        case Some(value) => endQuote(value)
        case None => Pos(Left(InvalidEscapeCharacter), reader.prevIndex)
      }

    def endQuote(value: Char): Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          if (ch == '\'') Pos(Right(Token.CharLiteral(value)), begin, reader.prevIndex)
          else {
            reader.unget(ch)
            Pos(Left(UnclosedCharacterLiteral), begin, reader.prevIndex + 1)
          }
        case _ => Pos(Left(UnclosedCharacterLiteral), begin, reader.prevIndex)
      }

    inChar
  }

  /**
   * Assumes the opening " has already been consumed
   */
  def stringLiteral(reader: CharReader,
                    buffer: CharBuffer): Pos[Either[ScannerError, Token]] = {
    val begin = reader.prevIndex
    buffer.reset()

    def q1: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => q2
            case '\n' =>
              reader.unget(ch)
              unclosed
            case '\\' => escapeSequence(multiLineMode = false)
            case other =>
              buffer.write(other)
              inStr
          }
        case None => unclosed
      }

    @tailrec
    def inStr: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => Pos(Right(StringLiteral(buffer.slice())), begin, reader.prevIndex)
            case '\n' =>
              reader.unget(ch)
              unclosed
            case '\\' => escapeSequence(multiLineMode = false)
            case other =>
              buffer.write(other)
              inStr
          }
        case None => unclosed
      }

    def q2: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          if (ch == '"') inMultiLineStr
          else {
            reader.unget(ch)
            Pos(Right(StringLiteral(buffer.slice())), begin, reader.prevIndex)
          }
        case None => Pos(Right(StringLiteral(buffer.slice())), begin, reader.prevIndex)
      }

    @tailrec
    def inMultiLineStr: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => endQ1
            case '\\' => escapeSequence(multiLineMode = true)
            case other =>
              buffer.write(other)
              inMultiLineStr
          }
        case None => unclosedMultiLine
      }

    def endQ1: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => endQ2
            case '\\' =>
              buffer.write('"')
              escapeSequence(multiLineMode = true)
            case other =>
              buffer.write('"')
              buffer.write(other)
              inMultiLineStr
          }
        case None => unclosedMultiLine
      }

    def endQ2: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => Pos(Right(MultiLineString(buffer.slice())), begin, reader.prevIndex)
            case '\\' =>
              buffer.write('"')
              buffer.write('"')
              escapeSequence(multiLineMode = true)
            case other =>
              buffer.write('"')
              buffer.write('"')
              buffer.write(other)
              inMultiLineStr
          }
        case None => unclosedMultiLine
      }

    def escapeSequence(multiLineMode: Boolean): Pos[Either[ScannerError, Token]] =
      EscapeSequence.scan(reader) match {
        case Some(value) =>
          buffer.write(value)
          if (multiLineMode) inMultiLineStr else inStr
        case None => Pos(Left(InvalidEscapeCharacter), reader.prevIndex)
      }

    def unclosed: Pos[Either[ScannerError, Token]] =
      Pos(Left(UnclosedStringLiteral), reader.prevIndex)

    def unclosedMultiLine: Pos[Either[ScannerError, Token]] =
      Pos(Left(UnclosedMultiLineString), reader.prevIndex)

    q1
  }

  /**
   * Assumes empty buffer
   */
  def tryNumericLiteral(reader: CharReader,
                        buffer: CharBuffer): Option[Pos[Either[ScannerError, Token]]] = {
    val negative = reader.tryGet('-')
    val leadingDecimalPoint = reader.tryGet('.')

    def rollBack(): Unit = {
      if (leadingDecimalPoint) reader.unget('.')
      if (negative) reader.unget('-')
    }

    reader.get() match {
      case Some(ch) =>
        if (isDigit(ch)) Some(numericLiteral(negative = negative, leadingDecimalPoint = leadingDecimalPoint,
          firstDigit = ch, reader, buffer))
        else {
          reader.unget(ch)
          rollBack()
          None
        }

      case None =>
        rollBack()
        None
    }
  }

  private def numericLiteral(negative: Boolean,
                             leadingDecimalPoint: Boolean,
                             firstDigit: Char,
                             reader: CharReader,
                             buffer: CharBuffer): Pos[Either[ScannerError, Token]] = {
    buffer.reset()
    var begin = reader.prevIndex
    if (leadingDecimalPoint) begin -= 1
    if (negative) begin -= 1

    @tailrec
    def leadingZero(first: Boolean,
                    wasSeparator: Boolean): Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          if (wasSeparator) {
            if (ch == '_') leadingZero(first, wasSeparator = true)
            else if (ch == '0') leadingZero(first, wasSeparator = false)
            else if (isDigit(ch)) {
              reader.unget(ch)
              leadingZero(first, wasSeparator = false)
            } else {
              reader.unget(ch)
              illegalSeparator
            }
          } else (ch: @switch) match {
            case 'x' | 'X' =>
              if (first) hex(0, 0, wasSeparator = false)
              else invalidLiteralNumber
            case 'b' | 'B' =>
              if (first) binary(0, 0, wasSeparator = false)
              else invalidLiteralNumber
            case '.' =>
              reader.get() match {
                case Some(c1) =>
                  if (c1.isDigit) {
                    rightOfDecimalPoint(1, wasSeparator = false)
                  } else {
                    reader.unget(c1)
                    reader.unget('.')
                    makeInteger
                  }
                case None =>
                  buffer.write('0')
                  makeInteger
              }
            case '0' => leadingZero(first = false, wasSeparator = false)
            case '_' => leadingZero(first = false, wasSeparator = true)
            case other =>
              if (isSuffix(other)) {
                buffer.write('0')
                beginSuffix(ch, beforeDecimalPoint = true)
              } else if (isDigit(other)) {
                buffer.write(ch)
                leftOfDecimalPoint(wasSeparator = false)
              } else if (isLetter(other)) {
                Pos(Left(InvalidLiteralNumber), begin, reader.prevIndex)
              } else {
                reader.unget(ch)
                Pos(Right(IntLiteral(0)), begin, reader.prevIndex)
              }
          }
        case None =>
          if (wasSeparator) illegalSeparator
          else Pos(Right(IntLiteral(0)), begin, reader.prevIndex)
      }

    @tailrec
    def leftOfDecimalPoint(wasSeparator: Boolean): Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          if (wasSeparator) {
            if (ch == '_') leftOfDecimalPoint(wasSeparator = true)
            else if (isDigit(ch)) {
              reader.unget(ch)
              leftOfDecimalPoint(wasSeparator = false)
            } else {
              reader.unget(ch)
              illegalSeparator
            }
          } else (ch: @switch) match {
            case '.' =>
              reader.get() match {
                case Some(c1) =>
                  if (isDigit(c1)) {
                    buffer.write('.')
                    buffer.write(c1)
                    rightOfDecimalPoint(1, wasSeparator = false)
                  } else {
                    reader.unget(c1)
                    makeInteger
                  }
                case None =>
                  reader.unget(ch)
                  makeInteger
              }
            case 'e' | 'E' =>
              reader.get() match {
                case Some(c1) =>
                  if (isDigit(c1)) {
                    buffer.write('E')
                    buffer.write(c1)
                    exponent
                  } else {
                    invalidLiteralNumber
                  }
                case None => invalidLiteralNumber
              }
            case '_' => leftOfDecimalPoint(wasSeparator = true)
            case _ =>
              if (isSuffix(ch)) {
                beginSuffix(ch, beforeDecimalPoint = true)
              } else if (isDigit(ch)) {
                buffer.write(ch)
                leftOfDecimalPoint(wasSeparator = false)
              } else if (isLetter(ch)) {
                invalidLiteralNumber
              } else {
                reader.unget(ch)
                makeInteger
              }
          }
        case None =>
          if (wasSeparator) illegalSeparator
          else makeInteger
      }

    def rightOfDecimalPoint(digitCount: Int,
                            wasSeparator: Boolean): Pos[Either[ScannerError, Token]] = ???

    def exponent: Pos[Either[ScannerError, Token]] = ???

    @tailrec
    def binary(acc: Long, size: Int, wasSeparator: Boolean): Pos[Either[ScannerError, Token]] =
      if (size > 64) integerTooLarge
      else reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '0' => binary(acc << 1, size + 1, wasSeparator = false)
            case '1' => binary((acc << 1) | 1, size + 1, wasSeparator = false)
            case '_' => binary(acc, size, wasSeparator = true)
            case 'l' | 'L' =>
              if (wasSeparator) illegalSeparator
              else Pos(Right(LongLiteral(maybeNegate(acc))), begin, reader.prevIndex)
            case other =>
              if (wasSeparator) illegalSeparator
              else if (isDigit(ch)) invalidLiteralNumber
              else if (size > 32) integerTooLarge
              else {
                reader.unget(other)
                Pos(Right(IntLiteral(maybeNegate(acc.toInt))), begin, reader.prevIndex)
              }
          }
        case None =>
          if (size < 1) invalidLiteralNumber
          else if (wasSeparator) illegalSeparator
          else if (size > 32) integerTooLarge
          else Pos(Right(IntLiteral(maybeNegate(acc.toInt))), begin, reader.prevIndex)
      }

    @tailrec
    def hex(acc: Long, size: Int, wasSeparator: Boolean): Pos[Either[ScannerError, Token]] =
      if (size > 16) integerTooLarge
      else reader.get() match {
        case Some(ch) =>
          val digitValue = HexDigits.digitValue(ch)
          if (digitValue >= 0) {
            hex((acc << 4) | (digitValue & 0xf), size + 1, wasSeparator = false)
          } else (ch: @switch) match {
            case '_' => hex(acc, size, wasSeparator = true)
            case 'l' | 'L' =>
              if (wasSeparator) illegalSeparator
              else Pos(Right(LongLiteral(maybeNegate(acc))), begin, reader.prevIndex)
            case other =>
              if (wasSeparator) illegalSeparator
              else if (isLetter(ch)) invalidLiteralNumber
              else if (size > 8) integerTooLarge
              else {
                reader.unget(other)
                Pos(Right(IntLiteral(maybeNegate(acc.toInt))), begin, reader.prevIndex)
              }
          }
        case None =>
          if (size < 1) invalidLiteralNumber
          else if (wasSeparator) illegalSeparator
          else if (size > 8) integerTooLarge
          else Pos(Right(IntLiteral(maybeNegate(acc.toInt))), begin, reader.prevIndex)
      }

    def beginSuffix(ch: Char,
                    beforeDecimalPoint: Boolean): Pos[Either[ScannerError, Token]] =
      if (reader.peek().exists(c => isLetter(c) || isDigit(c))) {
        invalidLiteralNumber
      } else (ch: @switch) match {
        case 'l' | 'L' =>
          if (beforeDecimalPoint) makeLong
          else invalidLiteralNumber
        case 'd' | 'D' =>
          makeDouble
        case 'f' | 'F' =>
          makeFloat
        case _ => invalidLiteralNumber
      }

    def makeInteger: Pos[Either[ScannerError, Token]] =
      if (buffer.size > 10) integerTooLarge
      else buffer.slice().toLongOption match {
        case Some(value) =>
          val resultAsLong = maybeNegate(value)
          if (resultAsLong >= Int.MinValue && resultAsLong <= Int.MaxValue) {
            Pos(Right(IntLiteral(resultAsLong.toInt)), begin, reader.prevIndex)
          } else {
            integerTooLarge
          }
        case None => invalidLiteralNumber // shouldn't happen
      }

    def makeLong: Pos[Either[ScannerError, Token]] = {
      val size = buffer.size
      if (size > 19) integerTooLarge
      else buffer.slice().toLongOption match {
        case Some(value) =>
          val resultAsLong = maybeNegate(value)
          if (resultAsLong >= Int.MinValue && resultAsLong <= Int.MaxValue) {
            Pos(Right(IntLiteral(resultAsLong.toInt)), begin, reader.prevIndex)
          } else {
            integerTooLarge
          }
        case None =>
          if (buffer.size == 19) integerTooLarge
          else invalidLiteralNumber // shouldn't happen
      }
    }

    def makeDouble: Pos[Either[ScannerError, Token]] = ???

    def makeFloat: Pos[Either[ScannerError, Token]] = ???

    def integerTooLarge: Pos[Either[ScannerError, Token]] =
      Pos(Left(IntegerNumberTooLarge), begin, reader.prevIndex)

    def illegalSeparator: Pos[Either[ScannerError, Token]] =
      Pos(Left(IllegalSeparator), reader.prevIndex)

    def invalidLiteralNumber: Pos[Either[ScannerError, Token]] =
      Pos(Left(InvalidLiteralNumber), begin, reader.prevIndex)

    def maybeNegate[A: Numeric](value: A): A =
      if (negative) implicitly[Numeric[A]].negate(value)
      else value

    if (leadingDecimalPoint) {
      buffer.write('0')
      buffer.write('.')
      buffer.write(firstDigit)
      rightOfDecimalPoint(1, wasSeparator = false)
    } else if (firstDigit == '0') {
      leadingZero(first = true, wasSeparator = false)
    } else {
      buffer.write(firstDigit)
      leftOfDecimalPoint(wasSeparator = false)
    }
  }

  private def isSuffix(ch: Char): Boolean =
    (ch: @switch) match {
      case 'd' | 'D' | 'l' | 'L' | 'f' | 'F' => true
      case _ => false
    }

  def main(args: Array[String]): Unit = {
    val z = 1
    val x = "9223372036854775807".toLongOption
    println(1.123456789_012345678e-123)
    // 9223372036854775807
    // 2147483647


    // 308 left of decimal point
    println(Long.MinValue)
    println(Long.MaxValue)

    println((-1).toChar)
    // Long: 19 digits
    // Int: 10 digits
  }
}
