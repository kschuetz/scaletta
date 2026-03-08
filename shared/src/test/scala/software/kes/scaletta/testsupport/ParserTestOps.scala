package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast.Expression
import software.kes.scaletta.parser.{ParseError, ParseOptions, ParseWarning}
import software.kes.scaletta.reporting.{LineMap, Pos}
import software.kes.scaletta.util.functional.Id._

object ParserTestOps {

  implicit class ParserStringOps(val input: String) extends AnyVal {

    def shouldParseTo(expected: Expression[Id])
                     (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import matchers._
      val result = support.parse(input, ParseOptions(requireExhaustion = true))
      if (result.errors.nonEmpty) {
        val errorMsg = result.errors.map(e => s"${e.value} at ${e.begin.value}").mkString(", ")
        fail(s"Parser errors for input '$input': $errorMsg")
      }
      if (result.warnings.nonEmpty) {
        val warningMsg = result.warnings.map(w => s"${w.value} at ${w.begin.value}").mkString(", ")
        fail(s"Unexpected parser warnings for input '$input': $warningMsg")
      }
      result.value match {
        case Some(v) => v.value.mapK(support.posToId) shouldBe expected
        case None => fail(s"Parser returned no value for input: $input")
      }
    }

    def shouldParsePartiallyTo(expected: Expression[Id])
                              (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): Unit = {
      import matchers._
      support.parseValue(input, ParseOptions(requireExhaustion = false)) shouldBe expected
    }

    def shouldParseWithWarnings(expected: ParseWarningMatchers.WarningWithPosition*)
                               (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ParseResultVerifier = {
      import ParseWarningMatchers._
      import matchers._
      val diag = support.parseWithDiagnostics(input, ParseOptions(requireExhaustion = true))
      if (diag.errors.nonEmpty) {
        val errorMsg = diag.errors.map(e => s"${e.value} at ${e.begin.value}").mkString(", ")
        fail(s"Unexpected parser errors for input '$input': $errorMsg")
      }
      diag.warnings should matchExactlyWarnings(input, diag.lineMap, expected.toVector)
      new ParseResultVerifier(input, diag.ast, diag.errors, diag.warnings, diag.lineMap)
    }

    def shouldFailWith(expected: ParseErrorMatchers.ErrorWithPosition)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ParseResultVerifier = {
      shouldFailWith(Vector(expected): _*)
    }

    def shouldFailWith(expectedErrors: ParseErrorMatchers.ErrorWithPosition*)
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ParseResultVerifier = {
      import ParseErrorMatchers._
      import matchers._
      val diag = support.parseWithDiagnostics(input)
      diag.errors should matchExactlyErrors(input, diag.lineMap, expectedErrors.toVector)
      new ParseResultVerifier(input, diag.ast, diag.errors, diag.warnings, diag.lineMap)
    }

    def shouldFailWith(matcher: Matcher[Vector[Pos[ParseError]]])
                      (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ParseResultVerifier = {
      import matchers._
      val diag = support.parseWithDiagnostics(input)
      diag.errors should matcher
      new ParseResultVerifier(input, diag.ast, diag.errors, diag.warnings, diag.lineMap)
    }

    /**
     * Asserts that parsing the input results in specific errors but still recovers
     * to produce a partial AST.
     *
     * This is a semantic alias for [[shouldFailWith]] that highlights the recovery aspect.
     */
    def shouldRecoverWith(expectedErrors: ParseErrorMatchers.ErrorWithPosition*)
                         (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ParseResultVerifier = {
      shouldFailWith(expectedErrors: _*)
    }

    /**
     * Asserts that parsing the input results in specific errors but still recovers
     * to produce a partial AST.
     *
     * This is a semantic alias for [[shouldFailWith]] that highlights the recovery aspect.
     */
    def shouldRecoverWith(matcher: Matcher[Vector[Pos[ParseError]]])
                         (implicit support: ParserTestSupport, matchers: Matchers, pos: Position): ParseResultVerifier = {
      shouldFailWith(matcher)
    }
  }

  class ParseResultVerifier(val input: String,
                            val actualAst: Option[Expression[Id]],
                            val actualErrors: Vector[Pos[ParseError]],
                            val actualWarnings: Vector[Pos[ParseWarning]],
                            val lineMap: LineMap) {

    /**
     * Asserts that the parser produced the expected (partial) AST during error recovery.
     *
     * @param expectedAst the AST structure expected from the parser
     * @return this verifier, allowing for chained assertions
     */
    def producing(expectedAst: Expression[Id])
                 (implicit matchers: Matchers, pos: Position): ParseResultVerifier = {
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
     * Asserts that the parser produced specific warnings.
     *
     * @param expectedWarnings the warnings expected from the parser
     * @return this verifier, allowing for chained assertions
     */
    def withWarnings(expectedWarnings: ParseWarningMatchers.WarningWithPosition*)
                    (implicit matchers: Matchers, pos: Position): ParseResultVerifier = {
      import ParseWarningMatchers._
      import matchers._
      actualWarnings should matchExactlyWarnings(input, lineMap, expectedWarnings.toVector)
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
    def ignoringAst(): ParseResultVerifier = this

    /**
     * Performs a custom assertion on the recovered partial AST.
     *
     * @param assertion a function that receives the partial AST and performs assertions on it
     * @return this verifier
     */
    def withPartialAst(assertion: Expression[Id] => Unit)
                      (implicit matchers: Matchers, pos: Position): ParseResultVerifier = {
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
                        (implicit matchers: Matchers, pos: Position): ParseResultVerifier = {
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
