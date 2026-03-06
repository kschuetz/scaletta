package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast.Expression
import software.kes.scaletta.util.functional.Id._

object ParserTestOps {

  implicit class ParserStringOps(val input: String) extends AnyVal {

    def shouldParseTo(expected: Expression[Id])
                     (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import matchers._
      support.parseValue(input) shouldBe expected
    }

    def shouldFailWith(expected: ParseErrorMatchers.ErrorWithPosition)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import ParseErrorMatchers._
      import matchers._
      val (_, errors) = support.parseWithErrors(input)
      errors should containError(expected)
    }

    def shouldFailWith(expectedErrors: ParseErrorMatchers.ErrorWithPosition*)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import ParseErrorMatchers._
      import matchers._
      val (_, errors) = support.parseWithErrors(input)
      expectedErrors.foreach { expected =>
        errors should containError(expected)
      }
    }
  }

}
