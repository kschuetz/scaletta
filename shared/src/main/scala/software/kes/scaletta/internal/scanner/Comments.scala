package software.kes.scaletta.internal.scanner

import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.CharIndex

import scala.annotation.switch

sealed trait CommentResult

object CommentResult {
  case object NoComment extends CommentResult

  sealed trait BlockComment extends CommentResult

  object BlockComment {
    case object SingleLine extends BlockComment

    case class MultiLine(indexOfLastNewLine: CharIndex) extends BlockComment
  }

  case class LineComment(indexOfNewLine: Option[CharIndex]) extends CommentResult

  case object Unterminated extends CommentResult
}

object Comments {

  def scanComments(reader: SourceReader): CommentResult = {
    if (reader.tryGet('/')) {
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '/' => scanLineComment(reader)
            case '*' => scanBlockComment(reader)
            case other =>
              reader.unget(other)
              reader.unget('/')
              CommentResult.NoComment
          }
        case None =>
          reader.unget('/')
          CommentResult.NoComment
      }
    } else CommentResult.NoComment
  }

  private def scanLineComment(reader: SourceReader): CommentResult = {
    var loop = true
    var indexOfNewLine: Option[CharIndex] = None
    while (loop) {
      reader.get() match {
        case Some('\n') =>
          val index = reader.prevIndex
          indexOfNewLine = Some(index)
          reader.unget('\n')
          loop = false
        case Some('\r') =>
          val index = reader.prevIndex
          val hasLF = reader.tryGet('\n')
          indexOfNewLine = Some(index)
          if (hasLF) reader.unget('\n')
          reader.unget('\r')
          loop = false
        case Some(_) => ()
        case None => loop = false
      }
    }
    CommentResult.LineComment(indexOfNewLine)
  }

  private def scanBlockComment(reader: SourceReader): CommentResult = {
    var depth = 1
    var loop = true
    var lastNewLine: Option[CharIndex] = None
    while (loop && depth > 0) {
      reader.get() match {
        case Some(c1) =>
          (c1: @switch) match {
            case '*' =>
              reader.get() match {
                case Some(c2) =>
                  if (c2 == '/') depth -= 1
                  else reader.unget(c2)
                case None =>
                  loop = false
              }
            case '\n' =>
              val index = reader.prevIndex
              lastNewLine = Some(index)
            case '\r' =>
              val index = reader.prevIndex
              reader.tryGet('\n')
              lastNewLine = Some(index)
            case '/' =>
              reader.get() match {
                case Some(c2) =>
                  if (c2 == '*') depth += 1
                  else reader.unget(c2)
                case None =>
                  loop = false
              }
            case _ => ()
          }
        case None => loop = false
      }
    }
    if (depth > 0) CommentResult.Unterminated
    else lastNewLine match {
      case Some(index) => CommentResult.BlockComment.MultiLine(index)
      case None => CommentResult.BlockComment.SingleLine
    }
  }
}
