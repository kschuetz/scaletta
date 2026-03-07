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

  class ContainErrorOfTypeMatcher[T <: ParseError](implicit classTag: scala.reflect.ClassTag[T]) extends Matcher[Vector[Pos[ParseError]]] {
    override def apply(left: Vector[Pos[ParseError]]): MatchResult = {
      val found = left.exists(p => classTag.runtimeClass.isInstance(p.value))
      MatchResult(
        found,
        s"Vector did not contain error of type ${classTag.runtimeClass.getSimpleName}. Actual errors: $left",
        s"Vector contained error of type ${classTag.runtimeClass.getSimpleName}"
      )
    }
  }

  class AtIndexMatcher(n: Int, inner: Matcher[Pos[ParseError]]) extends Matcher[Vector[Pos[ParseError]]] {
    override def apply(left: Vector[Pos[ParseError]]): MatchResult = {
      if (n < 0 || n >= left.length) {
        MatchResult(
          matches = false,
          s"Index $n is out of bounds for Vector of length ${left.length}. Actual errors: $left",
          ""
        )
      } else {
        val result = inner(left(n))
        MatchResult(
          result.matches,
          s"Error at index $n did not match: ${result.failureMessage}",
          s"Error at index $n matched: ${result.negatedFailureMessage}"
        )
      }
    }
  }

  class PosMatcher(inner: Matcher[ParseError]) extends Matcher[Pos[ParseError]] {
    override def apply(left: Pos[ParseError]): MatchResult = inner(left.value)
  }

  class ErrorTypeMatcher[T <: ParseError](implicit classTag: scala.reflect.ClassTag[T]) extends Matcher[ParseError] {
    override def apply(left: ParseError): MatchResult = {
      val matches = classTag.runtimeClass.isInstance(left)
      MatchResult(
        matches,
        s"Error $left was not of type ${classTag.runtimeClass.getSimpleName}",
        s"Error $left was of type ${classTag.runtimeClass.getSimpleName}"
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
            s"Unexpected extra error at index $i: ${act.value}\n" +
              renderUnderline(input, act.begin.value, "EXTRA"),
            ""
          )
        } else if (i >= actual.length) {
          val exp = expected(i)
          return MatchResult(
            matches = false,
            s"Missing expected error at index $i: ${exp.error}\n" +
              renderUnderline(input, exp.index, "MISSING"),
            ""
          )
        } else {
          val act = actual(i)
          val exp = expected(i)
          if (act.value != exp.error || act.begin.value != exp.index) {
            val message =
              s"""|Error mismatch at index $i:
                  |EXPECTED: ${exp.error} at index ${exp.index}
                  |${renderUnderline(input, exp.index, "EXPECTED")}
                  |ACTUAL:   ${act.value} at index ${act.begin.value}
                  |${renderUnderline(input, act.begin.value, "ACTUAL")}
                  |""".stripMargin
            return MatchResult(false, message, "")
          }
        }
      }
      MatchResult(matches = true, "", "Errors matched exactly")
    }
  }

  def containError(expected: ErrorWithPosition): ContainErrorMatcher =
    new ContainErrorMatcher(expected.error, expected.index)

  def containErrorOfType[T <: ParseError](implicit classTag: scala.reflect.ClassTag[T]): ContainErrorOfTypeMatcher[T] =
    new ContainErrorOfTypeMatcher[T]

  def errorOfType[T <: ParseError](implicit classTag: scala.reflect.ClassTag[T]): Matcher[Pos[ParseError]] =
    new PosMatcher(new ErrorTypeMatcher[T])

  def atIndex(n: Int)(inner: Matcher[Pos[ParseError]]): AtIndexMatcher =
    new AtIndexMatcher(n, inner)

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
