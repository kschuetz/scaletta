package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.Assertions
import software.kes.scaletta.scanner.{Pos, Token}
import software.kes.scaletta.testsupport.TestErrorFormatting.{formatToken, renderUnderline}

trait AssertExpectedTokens {
  assertions: Assertions =>

  final protected def assertExpectedTokens(input: String,
                                           expectedPosList: Vector[Pos[Token]],
                                           actualTokens: Vector[Pos[Token]])
                                          (implicit pos: Position): Unit = {
    val maxLength = Math.max(actualTokens.length, expectedPosList.length)
    for (i <- 0 until maxLength) {
      if (i >= expectedPosList.length) {
        val actual = actualTokens(i)
        fail(s"Unexpected extra token at index $i: ${formatToken(actual)}\n${renderUnderline(input, actual.begin.value, "extra token")}")
      } else if (i >= actualTokens.length) {
        val expected = expectedPosList(i)
        fail(s"Expected more tokens, but stream ended. Missing: ${formatToken(expected)}\n${renderUnderline(input, expected.begin.value, "missing expected token")}")
      } else {
        val actual = actualTokens(i)
        val expected = expectedPosList(i)

        if (actual.value != expected.value) {
          fail(s"Token mismatch at index $i:\nExpected: ${formatToken(expected)}\nActual:   ${formatToken(actual)}\n" +
            s"Context:\n${renderUnderline(input, expected.begin.value, "expected " + expected.value)}\n" +
            s"${renderUnderline(input, actual.begin.value, "actual " + actual.value)}")
        }

        if (actual.positionTuple != expected.positionTuple) {
          fail(s"Position mismatch for token '${actual.value}' at index $i:\nExpected: ${expected.begin}:${expected.end}\nActual:   ${actual.begin}:${actual.end}\n" +
            s"Context:\n${renderUnderline(input, expected.begin.value, "expected start")}\n" +
            s"${renderUnderline(input, actual.begin.value, "actual start")}")
        }
      }
    }
  }
}
