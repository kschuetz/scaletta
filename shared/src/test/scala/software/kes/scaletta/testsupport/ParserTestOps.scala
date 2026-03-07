package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast.Expression
import software.kes.scaletta.parser.{ParseError, ParseOptions}
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.util.functional.Id._

object ParserTestOps {

  implicit class ParserStringOps(val input: String) extends AnyVal {

    def shouldParseTo(expected: Expression[Id])
                     (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import matchers._
      support.parseValue(input, ParseOptions(requireExhaustion = true)) shouldBe expected
    }

    def shouldParsePartiallyTo(expected: Expression[Id])
                              (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import matchers._
      support.parseValue(input, ParseOptions(requireExhaustion = false)) shouldBe expected
    }

    def shouldFailWith(expected: ParseErrorMatchers.ErrorWithPosition)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      shouldFailWith(Vector(expected): _*)
    }

    def shouldFailWith(expectedErrors: ParseErrorMatchers.ErrorWithPosition*)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      import ParseErrorMatchers._
      import matchers._
      val (ast, errors, lineMap) = support.parseWithErrorsAndLineMap(input)
      errors should matchExactlyErrors(input, lineMap, expectedErrors.toVector)
      new ErrorResultVerifier(ast, errors)
    }

    def shouldFailWith(matcher: Matcher[Vector[Pos[ParseError]]])
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      import matchers._
      val (ast, errors, _) = support.parseWithErrorsAndLineMap(input)
      errors should matcher
      new ErrorResultVerifier(ast, errors)
    }

    /**
     * Asserts that parsing the input results in specific errors but still recovers
     * to produce a partial AST.
     *
     * This is a semantic alias for [[shouldFailWith]] that highlights the recovery aspect.
     */
    def shouldRecoverWith(expectedErrors: ParseErrorMatchers.ErrorWithPosition*)
                         (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      shouldFailWith(expectedErrors: _*)
    }

    /**
     * Asserts that parsing the input results in specific errors but still recovers
     * to produce a partial AST.
     *
     * This is a semantic alias for [[shouldFailWith]] that highlights the recovery aspect.
     */
    def shouldRecoverWith(matcher: Matcher[Vector[Pos[ParseError]]])
                         (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ErrorResultVerifier = {
      shouldFailWith(matcher)
    }
  }

  class ErrorResultVerifier(val actualAst: Option[Expression[Id]], val actualErrors: Vector[Pos[ParseError]]) {

    /**
     * Asserts that the parser produced the expected (partial) AST during error recovery.
     *
     * @param expectedAst the AST structure expected from the parser
     * @return this verifier, allowing for chained assertions
     */
    def producing(expectedAst: Expression[Id])
                 (implicit matchers: Matchers, pos: Position): ErrorResultVerifier = {
      import matchers._
      actualAst match {
        case Some(ast) =>
          AstDiff.diff(ast, expectedAst) match {
            case Some(mismatch) =>
              fail(s"AST mismatch: $mismatch")
            case None => // OK
          }
        case None =>
          fail(s"Expected AST $expectedAst, but parser returned None")
      }
      this
    }

    /**
     * Explicitly indicates that the partial AST should be ignored for this test case.
     *
     * Use this when the focus is solely on the presence of errors and the recovered AST
     * is either irrelevant or too complex to verify easily.
     *
     * @return this verifier
     */
    def ignoringAst(): ErrorResultVerifier = this

    /**
     * Performs a custom assertion on the recovered partial AST.
     *
     * @param assertion a function that receives the partial AST and performs assertions on it
     * @return this verifier
     */
    def withPartialAst(assertion: Expression[Id] => Unit)
                      (implicit matchers: Matchers, pos: Position): ErrorResultVerifier = {
      import matchers._
      actualAst match {
        case Some(ast) => assertion(ast)
        case None => fail("Parser returned no partial AST")
      }
      this
    }

    /**
     * Asserts that no "fatal" errors occurred during parsing that would have halted the analysis entirely.
     *
     * Currently checks for [[software.kes.scaletta.scanner.ScanError.UnbalancedBraces]].
     *
     * @return this verifier
     */
    def andNoFatalErrors()
                        (implicit matchers: Matchers, pos: Position): ErrorResultVerifier = {
      import matchers._
      import software.kes.scaletta.scanner.{ScanError, Token}
      actualErrors.foreach { p =>
        p.value match {
          case ParseError.UnexpectedToken(Token.Error(ScanError.UnbalancedBraces)) =>
            fail(s"Fatal error UnbalancedBraces found at index ${p.begin.value}")
          case _ => // OK
        }
      }
      this
    }
  }

}
