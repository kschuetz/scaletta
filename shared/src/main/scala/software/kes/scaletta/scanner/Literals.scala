package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerError._
import software.kes.scaletta.scanner.Token.{IntLiteral, LongLiteral, StringLiteral}
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object Literals {

  def charLiteral(reader: CharReader): Pos[Either[ScannerError, Token]] = {
    val begin = reader.prevIndex

    def inChar: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          if (ch == '\\') escapeSequence
          else if (ch == '\'') Pos(Left(EmptyCharacterLiteral), reader.prevIndex)
          else endQuote(ch)
        case None => Pos(Left(EmptyCharacterLiteral), reader.prevIndex)
      }

    def escapeSequence: Pos[Either[ScannerError, Token]] =
      EscapeSequence.scan(reader) match {
        case Some(value) => endQuote(value)
        case None => Pos(Left(InvalidEscapeCharacter), reader.prevIndex)
      }

    def endQuote(value: Char): Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) if ch == '\'' =>
          Pos(Right(Token.CharLiteral(value)), begin, reader.prevIndex)
        case _ => Pos(Left(UnclosedCharacterLiteral), reader.prevIndex)
      }

    inChar
  }

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
            case '"' => Pos(Right(StringLiteral(buffer.slice())), begin, reader.prevIndex)
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
  def numericLiteral(negative: Boolean,
                     leadingDecimalPoint: Boolean,
                     firstDigit: Char,
                     reader: CharReader,
                     buffer: CharBuffer): Pos[Either[ScannerError, Token]] = {
    var begin = reader.prevIndex
    if (leadingDecimalPoint) begin -= 1
    if (negative) begin -= 1

    def leadingZero: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case 'x' | 'X' => hex
            case 'b' | 'B' => binary(0, 0, wasSeparator = false)
            case '.' =>
              rightOfDecimalPoint
            case '0' | '_' =>
              leftOfDecimalPoint
            case 'd' | 'D' | 'l' | 'L' | 'f' | 'F' =>
              buffer.write('0')
              beginSuffix(ch)
            case other =>
              if (other.isDigit) {
                buffer.write(ch)
                leftOfDecimalPoint
              } else if (other.isLetter) {
                Pos(Left(InvalidLiteralNumber), begin)
              }
              ???
          }
        case None => ???
      }

    def leftOfDecimalPoint: Pos[Either[ScannerError, Token]] = ???


    def rightOfDecimalPoint: Pos[Either[ScannerError, Token]] = ???

    @tailrec
    def binary(acc: Long, size: Int, wasSeparator: Boolean): Pos[Either[ScannerError, Token]] =
      if (size > 64) tooLong
      else reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '0' => binary(acc << 1, size + 1, wasSeparator = false)
            case '1' => binary((acc << 1) | 1, size + 1, wasSeparator = false)
            case '_' => binary(acc, size, wasSeparator = true)
            case 'l' | 'L' =>
              if (wasSeparator) illegalSeparator
              else Pos(Right(LongLiteral(acc)), begin, reader.prevIndex)
            case other =>
              if (wasSeparator) illegalSeparator
              else if (ch.isDigit) invalidLiteralNumber
              else if (size > 32) tooLong
              else {
                reader.unget(other)
                Pos(Right(IntLiteral(acc.toInt)), begin, reader.prevIndex)
              }
          }
        case None =>
          if (size < 1) invalidLiteralNumber
          else if (wasSeparator) illegalSeparator
          else if (size > 32) tooLong
          else Pos(Right(IntLiteral(acc.toInt)), begin, reader.prevIndex)
      }

    def hex: Pos[Either[ScannerError, Token]] = ???

    def beginSuffix(char: Char): Pos[Either[ScannerError, Token]] = ???

    def tooLong: Pos[Either[ScannerError, Token]] = ???

    def illegalSeparator: Pos[Either[ScannerError, Token]] =
      Pos(Left(IllegalSeparator), reader.prevIndex)

    def invalidLiteralNumber: Pos[Either[ScannerError, Token]] =
      Pos(Left(InvalidLiteralNumber), begin, reader.prevIndex)

    if (leadingDecimalPoint) {
      buffer.write('0')
      buffer.write('.')
      buffer.write(firstDigit)
      rightOfDecimalPoint
    } else if (firstDigit == '0') {
      leadingZero
    } else {
      buffer.write(firstDigit)
      leftOfDecimalPoint
    }
  }


  def main(args: Array[String]): Unit = {
    val z = 123
    val x = 0b1_1
    println(x)
    // 9223372036854775807
    // 2147483647

    println((-1).toChar)
    // Long: 19 digits
    // Int: 10 digits
  }
}
