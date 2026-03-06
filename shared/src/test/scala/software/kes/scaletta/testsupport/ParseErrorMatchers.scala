package software.kes.scaletta.testsupport

import org.scalatest.matchers.{MatchResult, Matcher}
import software.kes.scaletta.parser.ParseError
import software.kes.scaletta.reporting.Pos

object ParseErrorMatchers {

  class ContainErrorMatcher(expectedError: ParseError, expectedBegin: Int) extends Matcher[Vector[Pos[ParseError]]] {
    override def apply(left: Vector[Pos[ParseError]]): MatchResult = {
      val found = left.exists(p => p.value == expectedError && p.begin.value == expectedBegin)
      MatchResult(
        found,
        s"Vector did not contain error $expectedError at index $expectedBegin. Actual errors: $left",
        s"Vector contained error $expectedError at index $expectedBegin"
      )
    }
  }

  def containError(expected: ErrorWithPosition): ContainErrorMatcher =
    new ContainErrorMatcher(expected.error, expected.index)

  case class ErrorWithPosition(error: ParseError, index: Int)

  implicit class ParseErrorOps(error: ParseError) {
    def at(index: Int): ErrorWithPosition = ErrorWithPosition(error, index)
  }

  def matchErrors(expected: Vector[Pos[ParseError]]): Matcher[Vector[Pos[ParseError]]] =
    (left: Vector[Pos[ParseError]]) => {
      MatchResult(
        left == expected,
        s"Errors $left did not match expected $expected",
        s"Errors $left matched expected $expected"
      )
    }
}

//object ParseErrorMatchers extends ParseErrorMatchers
