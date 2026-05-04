package software.kes.scaletta.internal.scanner

import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.scanner.ScanError._
import software.kes.scaletta.internal.scanner.ScannerConstants.{DoubleQuotes2, DoubleQuotes3}
import software.kes.scaletta.internal.scanner.Token._
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object InterpolatedStrings {
  type Result = Pos[Token]

  def scanPart(reader: SourceReader,
               buffer: CharBuffer,
               multiLine: Boolean,
               isRaw: Boolean): Result = {
    val begin = reader.currentIndex
    buffer.reset()

    def done: Result =
      Pos(InterpolatedPart(buffer.slice()), begin, reader.prevIndex)

    def unclosed: Result = {
      val error = if (multiLine) UnclosedMultiLineString else UnclosedStringLiteral
      Pos(Error(error), begin, reader.currentIndex)
    }

    @tailrec
    def skipToEnd(error: ScanError, errorPos: CharIndex): Result =
      reader.get() match {
        case Some('"') => Pos(Error(error), errorPos, errorPos)
        case Some(_) => skipToEnd(error, errorPos)
        case None => unclosed
      }

    @tailrec
    def skipToEndMulti(error: ScanError, errorPos: CharIndex): Result =
      reader.get() match {
        case Some('"') => if (reader.matchSequence(DoubleQuotes2)) {
          Pos(Error(error), errorPos, errorPos)
        } else skipToEndMulti(error, errorPos)
        case Some(_) => skipToEndMulti(error, errorPos)
        case None => unclosed
      }

    @tailrec
    def go(): Result =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' =>
              if (multiLine) {
                if (reader.tryGet('"')) {
                  if (reader.tryGet('"')) {
                    reader.ungetString(DoubleQuotes3)
                    done
                  } else {
                    buffer.write(DoubleQuotes2)
                    go()
                  }
                } else {
                  buffer.write('"')
                  go()
                }
              } else {
                reader.unget('"')
                done
              }
            case '$' =>
              reader.get() match {
                case Some('$') =>
                  buffer.write('$')
                  go()
                case Some(other) =>
                  reader.unget(other)
                  reader.unget('$')
                  done
                case None =>
                  reader.unget('$')
                  done
              }
            case '\\' =>
              if (isRaw) {
                buffer.write('\\')
                go()
              } else {
                EscapeSequence.scan(reader) match {
                  case EscapeResult.Success(escaped) =>
                    buffer.write(escaped)
                    go()
                  case EscapeResult.Error(error) =>
                    if (multiLine) {
                      skipToEndMulti(error, reader.prevIndex)
                    } else skipToEnd(error, reader.prevIndex)
                  case EscapeResult.Boundary =>
                    unclosed
                }
              }
            case '\n' =>
              if (multiLine) {
                buffer.write('\n')
                go()
              } else {
                reader.unget('\n')
                unclosed
              }
            case '\r' =>
              val hasLF = reader.tryGet('\n')
              if (multiLine) {
                buffer.write('\n')
                go()
              } else {
                if (hasLF) reader.unget('\n')
                reader.unget('\r')
                unclosed
              }
            case other =>
              buffer.write(other)
              go()
          }
        case None =>
          unclosed
      }

    go()
  }
}
