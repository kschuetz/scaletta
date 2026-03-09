package software.kes.scaletta.testsupport

import org.scalatest.matchers.{MatchResult, Matcher}
import software.kes.scaletta.parser.ParseHint
import software.kes.scaletta.reporting.{LineMap, Pos}
import software.kes.scaletta.testsupport.TestErrorFormatting.renderUnderline

object ParseHintMatchers {

  class ContainHintMatcher(expectedHint: ParseHint, expectedBegin: Int, expectedEnd: Option[Int] = None) extends Matcher[Vector[Pos[ParseHint]]] {
    override def apply(left: Vector[Pos[ParseHint]]): MatchResult = {
      val found = left.exists(p => p.value == expectedHint && p.begin.value == expectedBegin && expectedEnd.forall(_ == p.end.value))
      val expectedStr = expectedEnd.fold(s"at index $expectedBegin")(end => s"spanning ($expectedBegin, $end)")
      MatchResult(
        found,
        s"Vector did not contain hint $expectedHint $expectedStr. Actual hints: $left",
        s"Vector contained hint $expectedHint $expectedStr"
      )
    }
  }

  class ContainHintOfTypeMatcher[T <: ParseHint](implicit classTag: scala.reflect.ClassTag[T]) extends Matcher[Vector[Pos[ParseHint]]] {
    override def apply(left: Vector[Pos[ParseHint]]): MatchResult = {
      val found = left.exists(p => classTag.runtimeClass.isInstance(p.value))
      MatchResult(
        found,
        s"Vector did not contain hint of type ${classTag.runtimeClass.getSimpleName}. Actual hints: $left",
        s"Vector contained hint of type ${classTag.runtimeClass.getSimpleName}"
      )
    }
  }

  class AtIndexMatcher(n: Int, inner: Matcher[Pos[ParseHint]]) extends Matcher[Vector[Pos[ParseHint]]] {
    override def apply(left: Vector[Pos[ParseHint]]): MatchResult = {
      if (n < 0 || n >= left.length) {
        MatchResult(
          matches = false,
          s"Index $n is out of bounds for Vector of length ${left.length}. Actual hints: $left",
          ""
        )
      } else {
        val result = inner(left(n))
        MatchResult(
          result.matches,
          s"Hint at index $n did not match: ${result.failureMessage}",
          s"Hint at index $n matched: ${result.negatedFailureMessage}"
        )
      }
    }
  }

  class PosMatcher(inner: Matcher[ParseHint]) extends Matcher[Pos[ParseHint]] {
    override def apply(left: Pos[ParseHint]): MatchResult = inner(left.value)
  }

  class HintTypeMatcher[T <: ParseHint](implicit classTag: scala.reflect.ClassTag[T]) extends Matcher[ParseHint] {
    override def apply(left: ParseHint): MatchResult = {
      val matches = classTag.runtimeClass.isInstance(left)
      MatchResult(
        matches,
        s"Hint $left was not of type ${classTag.runtimeClass.getSimpleName}",
        s"Hint $left was of type ${classTag.runtimeClass.getSimpleName}"
      )
    }
  }

  class MatchExactlyHintsMatcher(input: String, lineMap: LineMap, expected: Vector[HintWithPosition]) extends Matcher[Vector[Pos[ParseHint]]] {
    override def apply(actual: Vector[Pos[ParseHint]]): MatchResult = {
      val maxLength = Math.max(actual.length, expected.length)
      for (i <- 0 until maxLength) {
        if (i >= expected.length) {
          val act = actual(i)
          return MatchResult(
            matches = false,
            s"Unexpected extra hint at index $i: ${act.value}\n" +
              renderUnderline(input, lineMap, act.begin.value, Some(act.end.value), "EXTRA"),
            ""
          )
        } else if (i >= actual.length) {
          val exp = expected(i)
          return MatchResult(
            matches = false,
            s"Missing expected hint at index $i: ${exp.hint}\n" +
              renderUnderline(input, lineMap, exp.index, exp.endIndex, "MISSING"),
            ""
          )
        } else {
          val act = actual(i)
          val exp = expected(i)
          if (act.value != exp.hint || act.begin.value != exp.index || exp.endIndex.exists(_ != act.end.value)) {
            val expEndStr = exp.endIndex.fold("")(e => s" to $e")
            val message =
              s"""|Hint mismatch at index $i:
                  |EXPECTED: ${exp.hint} at index ${exp.index}$expEndStr
                  |${renderUnderline(input, lineMap, exp.index, exp.endIndex, "EXPECTED")}
                  |ACTUAL:   ${act.value} at index ${act.begin.value} to ${act.end.value}
                  |${renderUnderline(input, lineMap, act.begin.value, Some(act.end.value), "ACTUAL")}
                  |""".stripMargin
            return MatchResult(matches = false, message, "")
          }
        }
      }
      MatchResult(matches = true, "", "Hints matched exactly")
    }
  }

  def containHint(expected: HintWithPosition): ContainHintMatcher =
    new ContainHintMatcher(expected.hint, expected.index, expected.endIndex)

  def containHintOfType[T <: ParseHint](implicit classTag: scala.reflect.ClassTag[T]): ContainHintOfTypeMatcher[T] =
    new ContainHintOfTypeMatcher[T]

  def hintOfType[T <: ParseHint](implicit classTag: scala.reflect.ClassTag[T]): Matcher[Pos[ParseHint]] =
    new PosMatcher(new HintTypeMatcher[T])

  def atIndex(n: Int)(inner: Matcher[Pos[ParseHint]]): AtIndexMatcher =
    new AtIndexMatcher(n, inner)

  def matchExactlyHints(input: String, lineMap: LineMap, expected: Vector[HintWithPosition]): MatchExactlyHintsMatcher =
    new MatchExactlyHintsMatcher(input, lineMap, expected)

  case class HintWithPosition(hint: ParseHint, index: Int, endIndex: Option[Int] = None)

  implicit class ParseHintOps(hint: ParseHint) {
    def at(index: Int): HintWithPosition = HintWithPosition(hint, index)

    def spanning(begin: Int, end: Int): HintWithPosition = HintWithPosition(hint, begin, Some(end))
  }

  def matchHints(expected: Vector[Pos[ParseHint]]): Matcher[Vector[Pos[ParseHint]]] =
    (left: Vector[Pos[ParseHint]]) => {
      MatchResult(
        left == expected,
        s"Hints $left did not match expected $expected",
        s"Hints $left matched expected $expected"
      )
    }
}
