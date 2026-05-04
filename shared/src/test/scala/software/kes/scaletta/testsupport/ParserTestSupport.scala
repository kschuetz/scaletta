package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.Assertions
import software.kes.scaletta.internal.ast.{Expression, TypeIdentifier}
import software.kes.scaletta.internal.parser._
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}
import software.kes.scaletta.util.functional.Id._
import software.kes.scaletta.util.functional.~>

/**
 * Provides common functionality for parser tests.
 *
 * This class encapsulates the setup of [[SourceReader]], [[Scanner]], and [[Parser]]
 * to ensure consistent environment configuration across tests.
 */
class ParserTestSupport() {
  private object assertions extends Assertions

  import assertions._

  object posToId extends (Pos ~> Id) {
    def apply[A](fa: Pos[A]): Id[A] = fa.value
  }

  case class ExprDiagnostics(ast: Option[Expression[Id]],
                             diagnostics: ParseDiagnostics,
                             lineMap: LineMap) {
    def errors: Vector[Pos[ParseError]] = diagnostics.errors

    def warnings: Vector[Pos[ParseWarning]] = diagnostics.warnings

    def hints: Vector[Pos[ParseHint]] = diagnostics.hints
  }

  case class TypeDiagnostics(ast: Option[TypeIdentifier[Id]],
                             diagnostics: ParseDiagnostics,
                             lineMap: LineMap) {
    def errors: Vector[Pos[ParseError]] = diagnostics.errors

    def warnings: Vector[Pos[ParseWarning]] = diagnostics.warnings

    def hints: Vector[Pos[ParseHint]] = diagnostics.hints
  }

  /**
   * Parses the input string as a type identifier and returns the full [[ParseResult]].
   */
  def parseType(input: String): TypeResult[Pos] = {
    val reader = SourceReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    TypeIdentifierParser.parse(scanner)
  }

  /**
   * Parses the input string as a type identifier and returns the AST with positional information stripped.
   */
  def parseTypeValue(input: String)(implicit pos: Position): TypeIdentifier[Id] = {
    val result = parseType(input)
    if (result.errors.nonEmpty) {
      val errorMsg = result.errors.map(e => s"${e.value} at ${e.begin.value}").mkString(", ")
      fail(s"Type parser errors for input '$input': $errorMsg")
    }
    result.value match {
      case Some(p) => p.value.mapK(posToId)
      case None => fail(s"Type parser returned no value for input: $input")
    }
  }

  /**
   * Parses the input string as a type identifier and returns the AST, any errors, warnings, and the line map.
   */
  def parseTypeWithDiagnostics(input: String): TypeDiagnostics = {
    val reader = SourceReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val result = TypeIdentifierParser.parse(scanner)
    TypeDiagnostics(
      result.value.map(_.value.mapK(posToId)),
      result.diagnostics,
      reader.lineMap
    )
  }

  /**
   * Parses the input string and returns the full [[ParseResult]].
   */
  def parse(input: String, options: ParseOptions = ParseOptions()): ExprResult[Pos] = {
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
  def parseWithDiagnostics(input: String, options: ParseOptions = ParseOptions()): ExprDiagnostics = {
    val reader = SourceReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    val result = parser.parse(scanner, options)
    ExprDiagnostics(
      result.value.map(_.value.mapK(posToId)),
      result.diagnostics,
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
