package software.kes.scaletta.testsupport

import org.scalatest.matchers.{MatchResult, Matcher}
import software.kes.scaletta.parser.ParseError
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.testsupport.TestErrorFormatting.renderUnderline

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

  class MatchExactlyErrorsMatcher(input: String, expected: Vector[ErrorWithPosition]) extends Matcher[Vector[Pos[ParseError]]] {
    override def apply(actual: Vector[Pos[ParseError]]): MatchResult = {
      val maxLength = Math.max(actual.length, expected.length)
      for (i <- 0 until maxLength) {
        if (i >= expected.length) {
          val act = actual(i)
          return MatchResult(
            matches = false,
            s"Unexpected extra error at index $i: ${act.value} at ${act.begin.value}\n${renderUnderline(input, act.begin.value, "extra error")}",
            ""
          )
        } else if (i >= actual.length) {
          val exp = expected(i)
          return MatchResult(
            matches = false,
            s"Expected more errors, but got only ${actual.length}. Missing: ${exp.error} at ${exp.index}\n${renderUnderline(input, exp.index, "missing expected error")}",
            ""
          )
        } else {
          val act = actual(i)
          val exp = expected(i)
          if (act.value != exp.error || act.begin.value != exp.index) {
            return MatchResult(
              matches = false,
              s"Error mismatch at index $i:\nExpected: ${exp.error} at ${exp.index}\nActual:   ${act.value} at ${act.begin.value}\n" +
                s"Context:\n${renderUnderline(input, exp.index, "expected " + exp.error)}\n" +
                s"${renderUnderline(input, act.begin.value, "actual " + act.value)}",
              ""
            )
          }
        }
      }
      MatchResult(matches = true, "", "Errors matched exactly")
    }
  }

  def containError(expected: ErrorWithPosition): ContainErrorMatcher =
    new ContainErrorMatcher(expected.error, expected.index)

  def matchExactlyErrors(input: String, expected: Vector[ErrorWithPosition]): MatchExactlyErrorsMatcher =
    new MatchExactlyErrorsMatcher(input, expected)

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
