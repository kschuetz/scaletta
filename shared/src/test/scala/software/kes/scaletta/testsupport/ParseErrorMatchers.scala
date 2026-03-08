package software.kes.scaletta.testsupport

import org.scalatest.matchers.{MatchResult, Matcher}
import software.kes.scaletta.parser.ParseError
import software.kes.scaletta.reporting.{LineMap, Pos}
import software.kes.scaletta.testsupport.TestErrorFormatting.renderUnderline

object ParseErrorMatchers {

  class ContainErrorMatcher(expectedError: ParseError, expectedBegin: Int, expectedEnd: Option[Int] = None) extends Matcher[Vector[Pos[ParseError]]] {
    override def apply(left: Vector[Pos[ParseError]]): MatchResult = {
      val found = left.exists(p => p.value == expectedError && p.begin.value == expectedBegin && expectedEnd.forall(_ == p.end.value))
      val expectedStr = expectedEnd.fold(s"at index $expectedBegin")(end => s"spanning ($expectedBegin, $end)")
      MatchResult(
        found,
        s"Vector did not contain error $expectedError $expectedStr. Actual errors: $left",
        s"Vector contained error $expectedError $expectedStr"
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

  class MatchExactlyErrorsMatcher(input: String, lineMap: LineMap, expected: Vector[ErrorWithPosition]) extends Matcher[Vector[Pos[ParseError]]] {
    override def apply(actual: Vector[Pos[ParseError]]): MatchResult = {
      val maxLength = Math.max(actual.length, expected.length)
      for (i <- 0 until maxLength) {
        if (i >= expected.length) {
          val act = actual(i)
          return MatchResult(
            matches = false,
            s"Unexpected extra error at index $i: ${act.value}\n" +
              renderUnderline(input, lineMap, act.begin.value, Some(act.end.value), "EXTRA"),
            ""
          )
        } else if (i >= actual.length) {
          val exp = expected(i)
          return MatchResult(
            matches = false,
            s"Missing expected error at index $i: ${exp.error}\n" +
              renderUnderline(input, lineMap, exp.index, exp.endIndex, "MISSING"),
            ""
          )
        } else {
          val act = actual(i)
          val exp = expected(i)
          if (act.value != exp.error || act.begin.value != exp.index || exp.endIndex.exists(_ != act.end.value)) {
            val expEndStr = exp.endIndex.fold("")(e => s" to $e")
            val message =
              s"""|Error mismatch at index $i:
                  |EXPECTED: ${exp.error} at index ${exp.index}$expEndStr
                  |${renderUnderline(input, lineMap, exp.index, exp.endIndex, "EXPECTED")}
                  |ACTUAL:   ${act.value} at index ${act.begin.value} to ${act.end.value}
                  |${renderUnderline(input, lineMap, act.begin.value, Some(act.end.value), "ACTUAL")}
                  |""".stripMargin
            return MatchResult(matches = false, message, "")
          }
        }
      }
      MatchResult(matches = true, "", "Errors matched exactly")
    }
  }

  def containError(expected: ErrorWithPosition): ContainErrorMatcher =
    new ContainErrorMatcher(expected.error, expected.index, expected.endIndex)

  def containErrorOfType[T <: ParseError](implicit classTag: scala.reflect.ClassTag[T]): ContainErrorOfTypeMatcher[T] =
    new ContainErrorOfTypeMatcher[T]

  def errorOfType[T <: ParseError](implicit classTag: scala.reflect.ClassTag[T]): Matcher[Pos[ParseError]] =
    new PosMatcher(new ErrorTypeMatcher[T])

  def atIndex(n: Int)(inner: Matcher[Pos[ParseError]]): AtIndexMatcher =
    new AtIndexMatcher(n, inner)

  def matchExactlyErrors(input: String, lineMap: LineMap, expected: Vector[ErrorWithPosition]): MatchExactlyErrorsMatcher =
    new MatchExactlyErrorsMatcher(input, lineMap, expected)

  def range[A](begin: Int, end: Int): Matcher[Pos[A]] = (left: Pos[A]) => {
    MatchResult(
      left.begin.value == begin && left.end.value == end,
      s"Pos range (${left.begin.value}, ${left.end.value}) did not match expected ($begin, $end)",
      s"Pos range matched expected ($begin, $end)"
    )
  }

  case class ErrorWithPosition(error: ParseError, index: Int, endIndex: Option[Int] = None)

  implicit class ParseErrorOps(error: ParseError) {
    def at(index: Int): ErrorWithPosition = ErrorWithPosition(error, index)

    def spanning(begin: Int, end: Int): ErrorWithPosition = ErrorWithPosition(error, begin, Some(end))
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
