package software.kes.scaletta.testsupport

import org.scalatest.matchers.{MatchResult, Matcher}
import software.kes.scaletta.internal.parser.ParseWarning
import software.kes.scaletta.reporting.{LineMap, Pos}
import software.kes.scaletta.testsupport.TestErrorFormatting.renderUnderline

object ParseWarningMatchers {

  class ContainWarningMatcher(expectedWarning: ParseWarning, expectedBegin: Int, expectedEnd: Option[Int] = None) extends Matcher[Vector[Pos[ParseWarning]]] {
    override def apply(left: Vector[Pos[ParseWarning]]): MatchResult = {
      val found = left.exists(p => p.value == expectedWarning && p.begin.value == expectedBegin && expectedEnd.forall(_ == p.end.value))
      val expectedStr = expectedEnd.fold(s"at index $expectedBegin")(end => s"spanning ($expectedBegin, $end)")
      MatchResult(
        found,
        s"Vector did not contain warning $expectedWarning $expectedStr. Actual warnings: $left",
        s"Vector contained warning $expectedWarning $expectedStr"
      )
    }
  }

  class ContainWarningOfTypeMatcher[T <: ParseWarning](implicit classTag: scala.reflect.ClassTag[T]) extends Matcher[Vector[Pos[ParseWarning]]] {
    override def apply(left: Vector[Pos[ParseWarning]]): MatchResult = {
      val found = left.exists(p => classTag.runtimeClass.isInstance(p.value))
      MatchResult(
        found,
        s"Vector did not contain warning of type ${classTag.runtimeClass.getSimpleName}. Actual warnings: $left",
        s"Vector contained warning of type ${classTag.runtimeClass.getSimpleName}"
      )
    }
  }

  class AtIndexMatcher(n: Int, inner: Matcher[Pos[ParseWarning]]) extends Matcher[Vector[Pos[ParseWarning]]] {
    override def apply(left: Vector[Pos[ParseWarning]]): MatchResult = {
      if (n < 0 || n >= left.length) {
        MatchResult(
          matches = false,
          s"Index $n is out of bounds for Vector of length ${left.length}. Actual warnings: $left",
          ""
        )
      } else {
        val result = inner(left(n))
        MatchResult(
          result.matches,
          s"Warning at index $n did not match: ${result.failureMessage}",
          s"Warning at index $n matched: ${result.negatedFailureMessage}"
        )
      }
    }
  }

  class PosMatcher(inner: Matcher[ParseWarning]) extends Matcher[Pos[ParseWarning]] {
    override def apply(left: Pos[ParseWarning]): MatchResult = inner(left.value)
  }

  class WarningTypeMatcher[T <: ParseWarning](implicit classTag: scala.reflect.ClassTag[T]) extends Matcher[ParseWarning] {
    override def apply(left: ParseWarning): MatchResult = {
      val matches = classTag.runtimeClass.isInstance(left)
      MatchResult(
        matches,
        s"Warning $left was not of type ${classTag.runtimeClass.getSimpleName}",
        s"Warning $left was of type ${classTag.runtimeClass.getSimpleName}"
      )
    }
  }

  class MatchExactlyWarningsMatcher(input: String, lineMap: LineMap, expected: Vector[WarningWithPosition]) extends Matcher[Vector[Pos[ParseWarning]]] {
    override def apply(actual: Vector[Pos[ParseWarning]]): MatchResult = {
      val maxLength = Math.max(actual.length, expected.length)
      for (i <- 0 until maxLength) {
        if (i >= expected.length) {
          val act = actual(i)
          return MatchResult(
            matches = false,
            s"Unexpected extra warning at index $i: ${act.value}\n" +
              renderUnderline(input, lineMap, act.begin.value, Some(act.end.value), "EXTRA"),
            ""
          )
        } else if (i >= actual.length) {
          val exp = expected(i)
          return MatchResult(
            matches = false,
            s"Missing expected warning at index $i: ${exp.warning}\n" +
              renderUnderline(input, lineMap, exp.index, exp.endIndex, "MISSING"),
            ""
          )
        } else {
          val act = actual(i)
          val exp = expected(i)
          if (act.value != exp.warning || act.begin.value != exp.index || exp.endIndex.exists(_ != act.end.value)) {
            val expEndStr = exp.endIndex.fold("")(e => s" to $e")
            val message =
              s"""|Warning mismatch at index $i:
                  |EXPECTED: ${exp.warning} at index ${exp.index}$expEndStr
                  |${renderUnderline(input, lineMap, exp.index, exp.endIndex, "EXPECTED")}
                  |ACTUAL:   ${act.value} at index ${act.begin.value} to ${act.end.value}
                  |${renderUnderline(input, lineMap, act.begin.value, Some(act.end.value), "ACTUAL")}
                  |""".stripMargin
            return MatchResult(matches = false, message, "")
          }
        }
      }
      MatchResult(matches = true, "", "Warnings matched exactly")
    }
  }

  def containWarning(expected: WarningWithPosition): ContainWarningMatcher =
    new ContainWarningMatcher(expected.warning, expected.index, expected.endIndex)

  def containWarningOfType[T <: ParseWarning](implicit classTag: scala.reflect.ClassTag[T]): ContainWarningOfTypeMatcher[T] =
    new ContainWarningOfTypeMatcher[T]

  def warningOfType[T <: ParseWarning](implicit classTag: scala.reflect.ClassTag[T]): Matcher[Pos[ParseWarning]] =
    new PosMatcher(new WarningTypeMatcher[T])

  def atIndex(n: Int)(inner: Matcher[Pos[ParseWarning]]): AtIndexMatcher =
    new AtIndexMatcher(n, inner)

  def matchExactlyWarnings(input: String, lineMap: LineMap, expected: Vector[WarningWithPosition]): MatchExactlyWarningsMatcher =
    new MatchExactlyWarningsMatcher(input, lineMap, expected)

  def range[A](begin: Int, end: Int): Matcher[Pos[A]] = (left: Pos[A]) => {
    MatchResult(
      left.begin.value == begin && left.end.value == end,
      s"Pos range (${left.begin.value}, ${left.end.value}) did not match expected ($begin, $end)",
      s"Pos range matched expected ($begin, $end)"
    )
  }

  case class WarningWithPosition(warning: ParseWarning, index: Int, endIndex: Option[Int] = None)

  implicit class ParseWarningOps(warning: ParseWarning) {
    def at(index: Int): WarningWithPosition = WarningWithPosition(warning, index)

    def spanning(begin: Int, end: Int): WarningWithPosition = WarningWithPosition(warning, begin, Some(end))
  }

  def matchWarnings(expected: Vector[Pos[ParseWarning]]): Matcher[Vector[Pos[ParseWarning]]] =
    (left: Vector[Pos[ParseWarning]]) => {
      MatchResult(
        left == expected,
        s"Warnings $left did not match expected $expected",
        s"Warnings $left matched expected $expected"
      )
    }
}
