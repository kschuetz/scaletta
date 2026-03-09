package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.Assertions
import software.kes.scaletta.ast.Expression
import software.kes.scaletta.parser._
import software.kes.scaletta.reader.SourceReader
import software.kes.scaletta.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.scanner.{IdentifierPolicy, Scanner}
import software.kes.scaletta.util.functional.Id._
import software.kes.scaletta.util.functional.~>

/**
 * Provides common functionality for parser tests.
 *
 * This class encapsulates the setup of [[SourceReader]], [[Scanner]], and [[Parser]]
 * to ensure consistent environment configuration across tests.
 */
class ParserTestSupport() {
  self: Assertions =>

  object posToId extends (Pos ~> Id) {
    def apply[A](fa: Pos[A]): Id[A] = fa.value
  }

  case class ParseDiagnostics(ast: Option[Expression[Id]],
                              errors: Vector[Pos[ParseError]],
                              warnings: Vector[Pos[ParseWarning]],
                              hints: Vector[Pos[ParseHint]],
                              lineMap: LineMap)

  /**
   * Parses the input string and returns the full [[ParseResult]].
   */
  def parse(input: String, options: ParseOptions = ParseOptions()): ParseResult[Pos] = {
    val reader = SourceReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    parser.parse(scanner, options)
  }

  /**
   * Parses the input string and returns the expression with positional information stripped.
   *
   * @throws org.scalatest.exceptions.TestFailedException if there are parse errors or if no value is returned.
   */
  def parseValue(input: String, options: ParseOptions = ParseOptions())
                (implicit pos: Position): Expression[Id] = {
    val result = parse(input, options)
    if (result.errors.nonEmpty) {
      val errorMsg = result.errors.map(e => s"${e.value} at ${e.begin.value}").mkString(", ")
      fail(s"Parser errors for input '$input': $errorMsg")
    }
    result.value match {
      case Some(pos) => pos.value.mapK(posToId)
      case None => fail(s"Parser returned no value for input: $input")
    }
  }

  /**
   * Parses the input string and returns the expression, any errors, warnings, and the line map.
   */
  def parseWithDiagnostics(input: String, options: ParseOptions = ParseOptions()): ParseDiagnostics = {
    val reader = SourceReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    val result = parser.parse(scanner, options)
    ParseDiagnostics(
      result.value.map(_.value.mapK(posToId)),
      result.errors,
      result.warnings,
      result.hints,
      reader.lineMap
    )
  }

  /**
   * Parses the input string and returns the (partial) expression, any errors, any warnings and the line map.
   */
  def parseWithErrorsAndLineMap(input: String, options: ParseOptions = ParseOptions()): (Option[Expression[Id]], Vector[Pos[ParseError]], Vector[Pos[ParseWarning]], Vector[Pos[ParseHint]], LineMap) = {
    val diag = parseWithDiagnostics(input, options)
    (diag.ast, diag.errors, diag.warnings, diag.hints, diag.lineMap)
  }

  /**
   * Parses the input string and returns both the (partial) expression, any errors, and any warnings.
   */
  def parseWithErrors(input: String, options: ParseOptions = ParseOptions()): (Option[Expression[Id]], Vector[Pos[ParseError]], Vector[Pos[ParseWarning]], Vector[Pos[ParseHint]]) = {
    val diag = parseWithDiagnostics(input, options)
    (diag.ast, diag.errors, diag.warnings, diag.hints)
  }
}
