package software.kes.scaletta.scanner

import software.kes.scaletta.reader.SourceReader
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.CharacterClass.{isDigit, isLetter}
import software.kes.scaletta.scanner.ScanError._
import software.kes.scaletta.scanner.Token._
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object Literals {

  // Maximum number of significant digits to the right of the decimal point
  // Anything beyond that is still parsed, but has no effect on the result.
  private val MaxFloatDigits = 20

  private val MinDoubleExponent = -324
  private val MaxDoubleExponent = 308
  private val MinSingleExponent = -45
  private val MaxSingleExponent = 38

  type Result = Pos[Token]

  /**
   * Assumes the opening ' has already been consumed
   */
  def charLiteral(reader: SourceReader): Result = {
    val begin = reader.prevIndex

    def inChar: Result =
      reader.get() match {
        case Some(ch) =>
          if (ch == '\\') escapeSequence
          else if (ch == '\'') Pos(Error(EmptyCharacterLiteral), begin, reader.prevIndex)
          else endQuote(ch)
        case None => Pos(Error(UnclosedCharacterLiteral), begin, reader.prevIndex)
      }

    def escapeSequence: Result =
      EscapeSequence.scan(reader) match {
        case EscapeResult.Success(value) => endQuote(value)
        case EscapeResult.Error(error) => Pos(Error(error), reader.prevIndex)
        case EscapeResult.Boundary => Pos(Error(UnclosedCharacterLiteral), begin, reader.prevIndex)
      }

    def endQuote(value: Char): Result =
      reader.get() match {
        case Some(ch) =>
          if (ch == '\'') Pos(Token.CharLiteral(value), begin, reader.prevIndex)
          else {
            reader.unget(ch)
            Pos(Error(UnclosedCharacterLiteral), begin, reader.prevIndex + 1)
          }
        case _ => Pos(Error(UnclosedCharacterLiteral), begin, reader.prevIndex)
      }

    inChar
  }

  /**
   * Assumes the opening " has already been consumed
   */
  def stringLiteral(reader: SourceReader,
                    buffer: CharBuffer): Result = {
    val begin = reader.prevIndex
    buffer.reset()

    def q1: Result =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => q2
            case '\n' =>
              reader.recordNewline(reader.currentIndex)
              reader.unget('\n')
              unclosed
            case '\r' =>
              val hasLF = reader.tryGet('\n')
              reader.recordNewline(reader.currentIndex)
              if (hasLF) reader.unget('\n')
              reader.unget('\r')
              unclosed
            case '\\' => escapeSequence(multiLineMode = false)
            case other =>
              buffer.write(other)
              inStr
          }
        case None => unclosed
      }

    @tailrec
    def inStr: Result =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => Pos(StringLiteral(buffer.slice()), begin, reader.prevIndex)
            case '\n' =>
              reader.recordNewline(reader.currentIndex)
              reader.unget('\n')
              unclosed
            case '\r' =>
              val hasLF = reader.tryGet('\n')
              reader.recordNewline(reader.currentIndex)
              if (hasLF) reader.unget('\n')
              reader.unget('\r')
              unclosed
            case '\\' => escapeSequence(multiLineMode = false)
            case other =>
              buffer.write(other)
              inStr
          }
        case None => unclosed
      }

    def q2: Result =
      reader.get() match {
        case Some(ch) =>
          if (ch == '"') inMultiLineStr
          else {
            reader.unget(ch)
            Pos(StringLiteral(buffer.slice()), begin, reader.prevIndex)
          }
        case None => Pos(StringLiteral(buffer.slice()), begin, reader.prevIndex)
      }

    @tailrec
    def inMultiLineStr: Result =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => endQ1
            case '\n' =>
              reader.recordNewline(reader.currentIndex)
              buffer.write('\n')
              inMultiLineStr
            case '\r' =>
              reader.tryGet('\n')
              reader.recordNewline(reader.currentIndex)
              buffer.write('\n')
              inMultiLineStr
            case '\\' => escapeSequence(multiLineMode = true)
            case other =>
              buffer.write(other)
              inMultiLineStr
          }
        case None => unclosedMultiLine
      }

    def endQ1: Result =
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

    def endQ2: Result =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' => Pos(MultiLineString(buffer.slice()), begin, reader.prevIndex)
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

    def escapeSequence(multiLineMode: Boolean): Result =
      EscapeSequence.scan(reader) match {
        case EscapeResult.Success(value) =>
          buffer.write(value)
          if (multiLineMode) inMultiLineStr else inStr
        case EscapeResult.Error(error) => Pos(Error(error), reader.prevIndex)
        case EscapeResult.Boundary => unclosed
      }

    def unclosed: Result = {
      reader.skipUntil(ch => ch == '\n' || ch == '\r')
      val res = reader.get()
      val end = res match {
        case Some(ch) =>
          reader.unget(ch)
          reader.prevIndex
        case None => reader.prevIndex
      }
      Pos(Error(UnclosedStringLiteral), begin, end)
    }

    def unclosedMultiLine: Result =
      Pos(Error(UnclosedMultiLineString), begin, reader.prevIndex)

    q1
  }

  /**
   * Assumes empty buffer
   */
  def tryNumericLiteral(reader: SourceReader,
                        buffer: CharBuffer): Option[Result] = {
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
                             reader: SourceReader,
                             buffer: CharBuffer): Result = {
    buffer.reset()
    var begin = reader.prevIndex
    if (leadingDecimalPoint) begin -= 1
    if (negative) begin -= 1

    def afterSeparator(ch: Char)
                      (k: Boolean => Result): Result =
      if (ch == '_') k(true)
      //      else if (ch == '0') k(false)
      else if (isDigit(ch)) {
        reader.unget(ch)
        k(false)
      } else {
        reader.unget(ch)
        illegalSeparator
      }

    def _leadingZero(first: Boolean)
                    (wasSeparator: Boolean): Result =
      leadingZero(first, wasSeparator)

    @tailrec
    def leadingZero(first: Boolean,
                    wasSeparator: Boolean): Result =
      reader.get() match {
        case Some(ch) =>
          if (wasSeparator) {
            afterSeparator(ch)(_leadingZero(first))
          } else (ch: @switch) match {
            case 'x' | 'X' =>
              if (first && !wasSeparator) hex(0, 0, wasSeparator = false)
              else invalidLiteralNumber
            case 'b' | 'B' =>
              if (first && !wasSeparator) binary(0, 0, wasSeparator = false)
              else invalidLiteralNumber
            case '.' =>
              reader.get() match {
                case Some(c1) =>
                  if (isDigit(c1)) {
                    buffer.write('0')
                    buffer.write('.')
                    buffer.write(c1)
                    rightOfDecimalPoint(1, wasSeparator = false)
                  } else {
                    reader.unget(c1)
                    reader.unget('.')
                    makeInteger
                  }
                case None =>
                  reader.unget('.')
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
                invalidLiteralNumber
              } else {
                reader.unget(ch)
                Pos(IntLiteral(0), begin, reader.prevIndex)
              }
          }
        case None =>
          if (wasSeparator) illegalSeparator
          else Pos(IntLiteral(0), begin, reader.prevIndex)
      }

    def _leftOfDecimalPoint(wasSeparator: Boolean): Result =
      leftOfDecimalPoint(wasSeparator)

    @tailrec
    def leftOfDecimalPoint(wasSeparator: Boolean): Result = {
      val temp1 = reader.prevIndex
      reader.get() match {
        case Some(ch) =>
          if (wasSeparator) {
            afterSeparator(ch)(_leftOfDecimalPoint)
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
                    reader.unget('.')
                    makeInteger
                  }
                case None =>
                  reader.unget(ch)
                  makeInteger
              }
            case 'e' | 'E' => tryExponent
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
    }

    def _rightOfDecimalPoint(digitCount: Int)
                            (wasSeparator: Boolean): Result =
      rightOfDecimalPoint(digitCount, wasSeparator)

    @tailrec
    def rightOfDecimalPoint(digitCount: Int,
                            wasSeparator: Boolean): Result =
      reader.get() match {
        case Some(ch) =>
          if (wasSeparator) {
            afterSeparator(ch)(_rightOfDecimalPoint(digitCount))
          } else (ch: @switch) match {
            case 'e' | 'E' => tryExponent
            case '_' => rightOfDecimalPoint(digitCount, wasSeparator = true)
            case _ =>
              if (isSuffix(ch)) {
                beginSuffix(ch, beforeDecimalPoint = false)
              } else if (isDigit(ch)) {
                if (digitCount <= MaxFloatDigits) {
                  buffer.write(ch)
                }
                rightOfDecimalPoint(digitCount + 1, wasSeparator = false)
              } else if (isLetter(ch)) {
                invalidLiteralNumber
              } else {
                reader.unget(ch)
                makeDouble
              }
          }
        case None =>
          if (wasSeparator) illegalSeparator
          else makeDouble
      }

    def tryExponent: Result =
      reader.get() match {
        case Some(c1) =>
          if (isDigit(c1)) {
            buffer.write('E')
            buffer.write(c1)
            exponent(c1 - '0', negative = false, wasSeparator = false)
          } else if (c1 == '-' || c1 == '+') {
            reader.get() match {
              case Some(c2) =>
                if (isDigit(c2)) {
                  val negative = c1 == '-'
                  buffer.write('E')
                  if (negative) buffer.write(c1)
                  buffer.write(c2)
                  exponent(c2 - '0', negative, wasSeparator = false)
                } else if (c2 == '_') {
                  illegalSeparator
                } else {
                  reader.unget(c2)
                  reader.unget(c1)
                  invalidLiteralNumber
                }
              case None =>
                reader.unget(c1)
                invalidLiteralNumber
            }
          } else if (c1 == '_') {
            illegalSeparator
          } else {
            reader.unget(c1)
            invalidLiteralNumber
          }
        case None => invalidLiteralNumber
      }

    def _exponent(acc: Int,
                  negative: Boolean)
                 (wasSeparator: Boolean): Result =
      exponent(acc, negative, wasSeparator)

    // Keeping track of the value in acc so we can report when precision has been exceeded
    def exponent(acc: Int,
                 negative: Boolean,
                 wasSeparator: Boolean): Result = {
      def checkPrecision(min: Int, max: Int)
                        (k: => Result): Result =
        if (negative) {
          if (-acc < min) floatingPointPrecisionTooSmall
          else k
        } else if (acc > max) floatingPointPrecisionTooLarge
        else k

      checkPrecision(MinDoubleExponent, MaxDoubleExponent) {
        reader.get() match {
          case Some(ch) =>
            if (wasSeparator) {
              afterSeparator(ch)(_exponent(acc, negative))
            } else if (isSuffix(ch)) {
              if (ch == 'f' || ch == 'F') {
                checkPrecision(MinSingleExponent, MaxSingleExponent) {
                  beginSuffix(ch, beforeDecimalPoint = false)
                }
              } else beginSuffix(ch, beforeDecimalPoint = false)
            } else if (ch == '_') {
              exponent(acc, negative, wasSeparator = true)
            } else if (isDigit(ch)) {
              if (!(ch == '0' && acc == 0)) {
                // don't bother to write leading zeroes
                buffer.write(ch)
              }
              val newAcc = (acc * 10) + (ch - '0')
              exponent(newAcc, negative, wasSeparator = false)
            } else if (isLetter(ch)) {
              invalidLiteralNumber
            } else {
              reader.unget(ch)
              makeDouble
            }
          case None =>
            if (wasSeparator) illegalSeparator
            else makeDouble
        }
      }
    }

    @tailrec
    def binary(acc: Long, size: Int, wasSeparator: Boolean): Result =
      if (size > 64) integerTooLarge
      else reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '0' => binary(acc << 1, size + 1, wasSeparator = false)
            case '1' => binary((acc << 1) | 1, size + 1, wasSeparator = false)
            case '_' => binary(acc, size, wasSeparator = true)
            case 'l' | 'L' =>
              if (wasSeparator) illegalSeparator
              else Pos(LongLiteral(maybeNegate(acc)), begin, reader.prevIndex)
            case other =>
              if (wasSeparator) illegalSeparator
              else if (isDigit(ch)) invalidLiteralNumber
              else if (size > 32) integerTooLarge
              else {
                reader.unget(other)
                Pos(IntLiteral(maybeNegate(acc.toInt)), begin, reader.prevIndex)
              }
          }
        case None =>
          if (size < 1) invalidLiteralNumber
          else if (wasSeparator) illegalSeparator
          else if (size > 32) integerTooLarge
          else Pos(IntLiteral(maybeNegate(acc.toInt)), begin, reader.prevIndex)
      }

    @tailrec
    def hex(acc: Long, size: Int, wasSeparator: Boolean): Result =
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
              else Pos(LongLiteral(maybeNegate(acc)), begin, reader.prevIndex)
            case other =>
              if (wasSeparator) illegalSeparator
              else if (isLetter(ch)) invalidLiteralNumber
              else if (size > 8) integerTooLarge
              else {
                reader.unget(other)
                Pos(IntLiteral(maybeNegate(acc.toInt)), begin, reader.prevIndex)
              }
          }
        case None =>
          if (size < 1) invalidLiteralNumber
          else if (wasSeparator) illegalSeparator
          else if (size > 8) integerTooLarge
          else Pos(IntLiteral(maybeNegate(acc.toInt)), begin, reader.prevIndex)
      }

    def beginSuffix(ch: Char,
                    beforeDecimalPoint: Boolean): Result =
      if (reader.peek().exists(c => isLetter(c) || isDigit(c))) {
        reader.skipUntil(c => !(isLetter(c) || isDigit(c)))
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

    def makeInteger: Result =
      if (buffer.size > 10) integerTooLarge
      else buffer.slice().toLongOption match {
        case Some(value) =>
          val resultAsLong = maybeNegate(value)
          if (resultAsLong >= Int.MinValue && resultAsLong <= Int.MaxValue) {
            Pos(IntLiteral(resultAsLong.toInt), begin, reader.prevIndex)
          } else {
            integerTooLarge
          }
        case None => invalidLiteralNumber // shouldn't happen
      }

    def makeLong: Result = {
      val size = buffer.size
      if (size > 19) integerTooLarge
      else {
        val s = if (negative && size >= 18) {
          buffer.insert(0, '-')
          buffer.slice()
        } else buffer.slice()
        s.toLongOption match {
          case Some(value) =>
            val result = if (size >= 18) value else maybeNegate(value)
            Pos(LongLiteral(result), begin, reader.prevIndex)
          case None =>
            if (size == 19) integerTooLarge
            else invalidLiteralNumber // shouldn't happen
        }
      }
    }

    def makeDouble: Result =
      buffer.slice().toDoubleOption match {
        case Some(value) =>
          val result = maybeNegate(value)
          if (result.isFinite) {
            Pos(DoubleLiteral(result), begin, reader.prevIndex)
          } else invalidLiteralNumber
        case None => invalidLiteralNumber
      }

    def makeFloat: Result =
      buffer.slice().toFloatOption match {
        case Some(value) =>
          val result = maybeNegate(value)
          if (result.isFinite) {
            Pos(FloatLiteral(result), begin, reader.prevIndex)
          } else invalidLiteralNumber
        case None => invalidLiteralNumber
      }

    def integerTooLarge: Result =
      Pos(Error(IntegerNumberTooLarge), begin, reader.prevIndex)

    def illegalSeparator: Result =
      Pos(Error(IllegalSeparator), reader.prevIndex)

    def invalidLiteralNumber: Result =
      Pos(Error(InvalidLiteralNumber), begin, reader.prevIndex)

    def floatingPointPrecisionTooSmall: Result =
      Pos(Error(FloatingPointPrecisionTooSmall), begin, reader.prevIndex)

    def floatingPointPrecisionTooLarge: Result =
      Pos(Error(FloatingPointPrecisionTooLarge), begin, reader.prevIndex)

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
    println("-9223372036854775808".toLongOption)
    val double = 1e308

    val _a = 123
    println(
      s""" ${_a}
      b""")

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
