package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast.Expression
import software.kes.scaletta.parser.ParseError
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.util.functional.Id._

object ParserTestOps {

  implicit class ParserStringOps(val input: String) extends AnyVal {

    def shouldParseTo(expected: Expression[Id])
                     (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import matchers._
      support.parseValue(input) shouldBe expected
    }

    def shouldFailWith(expected: ParseErrorMatchers.ErrorWithPosition)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      shouldFailWith(Vector(expected): _*)
    }

    def shouldFailWith(expectedErrors: ParseErrorMatchers.ErrorWithPosition*)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      import ParseErrorMatchers._
      import matchers._
      val (ast, errors) = support.parseWithErrors(input)
      errors should matchExactlyErrors(input, expectedErrors.toVector)
      new ErrorResultVerifier(ast)
    }

    def shouldFailWith(matcher: Matcher[Vector[Pos[ParseError]]])
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      import matchers._
      val (ast, errors) = support.parseWithErrors(input)
      errors should matcher
      new ErrorResultVerifier(ast)
    }
  }

  class ErrorResultVerifier(val actualAst: Option[Expression[Id]]) {
    def producing(expectedAst: Expression[Id])
                 (implicit matchers: Matchers, pos: Position): Unit = {
      import matchers._
      actualAst shouldBe Some(expectedAst)
    }
  }

}
