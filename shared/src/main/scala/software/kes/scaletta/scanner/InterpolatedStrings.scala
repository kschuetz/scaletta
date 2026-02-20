package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerError._
import software.kes.scaletta.scanner.Token._
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object InterpolatedStrings {
  type Result = Pos[Either[ScannerError, Token]]

  def scanPart(reader: CharReader,
               buffer: CharBuffer,
               multiLine: Boolean,
               isRaw: Boolean): Result = {
    val begin = reader.currentIndex
    buffer.reset()

    def done: Result =
      Pos(Right(InterpolatedPart(buffer.slice())), begin, reader.prevIndex)

    @tailrec
    def go(): Result =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '"' =>
              if (multiLine) {
                if (reader.tryGet('"')) {
                  if (reader.tryGet('"')) {
                    reader.unget('"')
                    reader.unget('"')
                    reader.unget('"')
                    done
                  } else {
                    buffer.write('"')
                    buffer.write('"')
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
                  case Some(escaped) =>
                    buffer.write(escaped)
                    go()
                  case None =>
                    Pos(Left(InvalidEscapeCharacter), reader.prevIndex)
                }
              }
            case '\n' =>
              if (multiLine) {
                buffer.write(ch)
                go()
              } else {
                reader.unget(ch)
                Pos(Left(UnclosedStringLiteral), begin, reader.currentIndex)
              }
            case other =>
              buffer.write(other)
              go()
          }
        case None =>
          Pos(Left(UnclosedStringLiteral), begin, reader.currentIndex)
      }

    go()
  }
}
