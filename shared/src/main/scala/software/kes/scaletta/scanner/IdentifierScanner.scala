package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerError.{EmptyQuotedIdentifier, IdentifierTooLong, InvalidEscapeCharacter, UnclosedQuotedIdentifier}
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

// TODO: Scala reserved word policy?
final class IdentifierScanner(policy: IdentifierPolicy) {

  /**
   * Assumes first character is already in buffer.
   * Can return one of the following:
   *   - Identifier (upper, lower, or operator)
   *   - Reserved word
   *   - BeginInterpolatedString/BeginMultiLineInterpolatedString
   */
  def plain(reader: CharReader,
            buffer: CharBuffer): Pos[Either[ScannerError, Token]] = {
    val begin = reader.prevIndex
    val firstChar = buffer.firstChar
    val isOperator = CharacterClass.isOperator(firstChar)

    @tailrec
    def normal(length: Int,
               wasUnderscore: Boolean): Pos[Either[ScannerError, Token]] =
      if (checkLength(length)) {
        reader.get() match {
          case Some(ch) =>
            if (CharacterClass.isIdentifierInner(ch)) {
              buffer.write(ch)
              normal(length + 1, ch == '_')
            } else if (wasUnderscore && CharacterClass.isOperator(ch) && buffer.size > 1) {
              buffer.write(ch)
              operator(length + 1)
            } else {
              reader.unget(ch)
              done
            }

          case None => done
        }
      } else Pos(Left(IdentifierTooLong), begin, reader.prevIndex)

    @tailrec
    def operator(length: Int): Pos[Either[ScannerError, Token]] =
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
            } else if (CharacterClass.isOperator(ch)) {
              buffer.write(ch)
              operator(length + 1)
            } else {
              reader.unget(ch)
              done
            }

          case None => done
        }
      } else Pos(Left(IdentifierTooLong), begin, reader.prevIndex)

    def done: Pos[Either[ScannerError, Token]] = {
      val name = buffer.slice()
      val end = reader.prevIndex
      Token.reservedWordByName.get(name) match {
        case Some(reservedWord) => Pos(Right(reservedWord), begin, end)
        case None =>
          if (isOperator) {
            construct(Token.Identifier.Operator.apply)(reader, buffer, begin)
          } else if (CharacterClass.isLetter(firstChar)) {
            doneStartsWithLetter
          } else {
            construct(Token.Identifier.Lower.apply)(reader, buffer, begin)
          }
      }
    }

    def doneStartsWithLetter: Pos[Either[ScannerError, Token]] = {
      def normal: Pos[Either[ScannerError, Token]] =
        if (CharacterClass.isUppercase(firstChar)) {
          construct(Token.Identifier.Upper.apply)(reader, buffer, begin)
        } else {
          construct(Token.Identifier.Lower.apply)(reader, buffer, begin)
        }

      if (reader.tryGet('"')) {
        if (reader.tryGet('"')) {
          if (reader.tryGet('"')) {
            construct(Token.BeginMultiLineInterpolatedString.apply)(reader, buffer, begin)
          } else {
            reader.unget('"')
            construct(Token.BeginInterpolatedString.apply)(reader, buffer, begin)
          }
        } else construct(Token.BeginInterpolatedString.apply)(reader, buffer, begin)
      } else normal
    }

    if (isOperator) {
      operator(1)
    } else {
      normal(1, buffer.firstChar == '_')
    }
  }


  /**
   * Assumes empty buffer
   */
  def quoted(reader: CharReader,
             buffer: CharBuffer): Pos[Either[ScannerError, Token]] = {
    val begin = reader.prevIndex

    @tailrec
    def go(length: Int): Pos[Either[ScannerError, Token]] =
      if (checkLength(length)) {
        reader.get() match {
          case Some(ch) =>
            (ch: @switch) match {
              case '\\' => escapeSequence(length)
              case '\n' => Pos(Left(UnclosedQuotedIdentifier), reader.prevIndex)
              case '`' =>
                if (buffer.isEmpty) Pos(Left(EmptyQuotedIdentifier), reader.prevIndex)
                else {
                  construct(Token.Identifier.Quoted.apply)(reader, buffer, begin)
                }
              case _ =>
                buffer.write(ch)
                go(length + 1)
            }
          case None => Pos(Left(UnclosedQuotedIdentifier), reader.prevIndex)
        }
      } else Pos(Left(IdentifierTooLong), begin, reader.prevIndex)

    def escapeSequence(length: Int): Pos[Either[ScannerError, Token]] =
      EscapeSequence.scan(reader) match {
        case Some(value) =>
          buffer.write(value)
          go(length + 1)
        case None => Pos(Left(InvalidEscapeCharacter), reader.prevIndex)
      }

    go(0)
  }

  private def construct(fn: String => Token)
                       (reader: CharReader,
                        buffer: CharBuffer,
                        begin: CharIndex): Pos[Either[ScannerError, Token]] = {
    val length = buffer.size
    if (policy.maxIdentifierLength.exists(max => length > max)) {
      Pos(Left(IdentifierTooLong), begin, reader.prevIndex)
    } else Pos(Right(fn(buffer.slice())), begin, reader.prevIndex)
  }

  private def checkLength(length: Int): Boolean = {
    length <= Token.maxReservedWordLength ||
      !policy.maxIdentifierLength.exists(max => length > max)
  }

}
