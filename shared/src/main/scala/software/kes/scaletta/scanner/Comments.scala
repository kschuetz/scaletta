package software.kes.scaletta.scanner

import scala.annotation.switch

sealed trait CommentResult

object CommentResult {
  case object NoComment extends CommentResult

  sealed trait BlockComment extends CommentResult

  object BlockComment {
    case object SingleLine extends BlockComment

    case object MultiLine extends BlockComment
  }

  case object LineComment extends CommentResult

  case object Unterminated extends CommentResult
}

object Comments {

  def scanComments(reader: CharReader): CommentResult = {
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

  private def scanLineComment(reader: CharReader): CommentResult = {
    reader.skipUntil(ch => ch == '\r' || ch == '\n')
    CommentResult.LineComment
  }

  private def scanBlockComment(reader: CharReader): CommentResult = {
    var depth = 1
    var loop = true
    var multiLine = false
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
              multiLine = true
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
    else if (multiLine) CommentResult.BlockComment.MultiLine
    else CommentResult.BlockComment.SingleLine
  }
}
