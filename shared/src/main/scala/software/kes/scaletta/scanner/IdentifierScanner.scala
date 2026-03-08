package software.kes.scaletta.scanner

import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.reader.SourceReader
import software.kes.scaletta.reporting.{CharIndex, Pos}
import software.kes.scaletta.scanner.CharacterClass._
import software.kes.scaletta.scanner.ScanError.{EmptyQuotedIdentifier, IdentifierTooLong, UnclosedQuotedIdentifier}
import software.kes.scaletta.scanner.Token.Error
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

// TODO: Scala reserved word policy?
final class IdentifierScanner(policy: IdentifierPolicy) {

  type Result = Pos[Token]

  /**
   * Can return one of the following:
   *   - Identifier (upper, lower, operator, or quoted)
   *   - Reserved word
   *   - BeginInterpolatedString/BeginMultiLineInterpolatedString
   */
  def tryScan(reader: SourceReader,
              buffer: CharBuffer): Option[Result] =
    reader.get() match {
      case Some(ch) =>
        if (ch == '`') Some {
          buffer.reset()
          quoted(reader, buffer)
        } else if (isIdentifierStart(ch) || isOperator(ch)) Some {
          buffer.reset()
          buffer.write(ch)
          plain(reader, buffer)
        } else {
          reader.unget(ch)
          None
        }
      case None => None
    }

  /**
   * Assumes first character is already in buffer.
   * Can return one of the following:
   *   - Identifier (upper, lower, or operator)
   *   - Reserved word
   *   - BeginInterpolatedString/BeginMultiLineInterpolatedString
   */
  private def plain(reader: SourceReader,
                    buffer: CharBuffer): Result = {
    val begin = reader.prevIndex
    val firstChar = buffer.firstChar
    val isOp = isOperator(firstChar)

    @tailrec
    def normal(length: Int,
               wasUnderscore: Boolean): Result =
      if (checkLength(length)) {
        reader.get() match {
          case Some(ch) =>
            if (isIdentifierInner(ch)) {
              buffer.write(ch)
              normal(length + 1, ch == '_')
            } else if (wasUnderscore && isOperator(ch) && buffer.size > 1) {
              buffer.write(ch)
              operator(length + 1)
            } else {
              reader.unget(ch)
              done
            }

          case None => done
        }
      } else Pos(Error(IdentifierTooLong), begin, reader.prevIndex)

    @tailrec
    def operator(length: Int): Result =
      if (checkLength(length)) {
        reader.get() match {
          case Some(ch) =>
            if (ch == '/') {
              // check if comment
              if (reader.peek().exists(c => c == '/' || c == '*')) {
                reader.unget(ch)
                done
              } else {
                buffer.write(ch)
                operator(length + 1)
              }
            } else if (isOperator(ch)) {
              buffer.write(ch)
              operator(length + 1)
            } else {
              reader.unget(ch)
              done
            }

          case None => done
        }
      } else Pos(Error(IdentifierTooLong), begin, reader.prevIndex)

    def done: Result = {
      val name = buffer.slice()
      val end = reader.prevIndex
      Token.reservedWordByName.get(name) match {
        case Some(reservedWord) => Pos(reservedWord, begin, end)
        case None =>
          if (isOp) {
            construct(Token.Identifier.Operator.apply)(reader, buffer, begin)
          } else if (isLetter(firstChar)) {
            doneStartsWithLetter
          } else {
            construct(Token.Identifier.Lower.apply)(reader, buffer, begin)
          }
      }
    }

    def doneStartsWithLetter: Result = {
      def normal: Result =
        if (isUppercase(firstChar)) {
          construct(Token.Identifier.Upper.apply)(reader, buffer, begin)
        } else {
          construct(Token.Identifier.Lower.apply)(reader, buffer, begin)
        }

      if (reader.matchSequence(ScannerConstants.DoubleQuotes3)) {
        construct { name =>
          Token.BeginMultiLineInterpolatedString(Interpolator.fromName(name))
        }(reader, buffer, begin)
      } else if (reader.tryGet('"')) {
        construct { name =>
          Token.BeginInterpolatedString(Interpolator.fromName(name))
        }(reader, buffer, begin)
      } else normal
    }

    if (isOp) {
      operator(1)
    } else {
      normal(1, buffer.firstChar == '_')
    }
  }

  /**
   * Assumes empty buffer
   */
  private def quoted(reader: SourceReader,
                     buffer: CharBuffer): Result = {
    val begin = reader.prevIndex

    @tailrec
    def go(length: Int): Result =
      if (checkLength(length)) {
        reader.get() match {
          case Some(ch) =>
            (ch: @switch) match {
              case '\\' => escapeSequence(length)
              case '\n' =>
                reader.unget('\n')
                Pos(Error(UnclosedQuotedIdentifier), begin, reader.prevIndex)
              case '`' =>
                if (buffer.isEmpty) Pos(Error(EmptyQuotedIdentifier), begin, reader.prevIndex)
                else {
                  construct(Token.Identifier.Quoted.apply)(reader, buffer, begin)
                }
              case _ =>
                buffer.write(ch)
                go(length + 1)
            }
          case None => Pos(Error(UnclosedQuotedIdentifier), begin, reader.prevIndex)
        }
      } else Pos(Error(IdentifierTooLong), begin, reader.prevIndex)

    def escapeSequence(length: Int): Result =
      EscapeSequence.scan(reader) match {
        case EscapeResult.Success(value) =>
          buffer.write(value)
          go(length + 1)
        case EscapeResult.Error(error) => Pos(Error(error), reader.prevIndex)
        case EscapeResult.Boundary => Pos(Error(UnclosedQuotedIdentifier), begin, reader.prevIndex)
      }

    go(0)
  }

  private def construct(fn: String => Token)
                       (reader: SourceReader,
                        buffer: CharBuffer,
                        begin: CharIndex): Result = {
    val length = buffer.size
    if (policy.maxIdentifierLength.exists(max => length > max)) {
      Pos(Error(IdentifierTooLong), begin, reader.prevIndex)
    } else Pos(fn(buffer.slice()), begin, reader.prevIndex)
  }

  private def checkLength(length: Int): Boolean = {
    length <= Token.maxReservedWordLength ||
      !policy.maxIdentifierLength.exists(max => length > max)
  }

}
