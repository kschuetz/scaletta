package software.kes.scaletta.testsupport

import org.scalactic.source.Position
import org.scalatest.Assertions
import software.kes.scaletta.ast.Expression
import software.kes.scaletta.parser.{ParseError, ParseResult, Parser}
import software.kes.scaletta.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.scanner.{CharReader, IdentifierPolicy, Scanner}
import software.kes.scaletta.util.functional.Id._
import software.kes.scaletta.util.functional.~>

/**
 * Provides common functionality for parser tests.
 *
 * This class encapsulates the setup of [[CharReader]], [[Scanner]], and [[Parser]]
 * to ensure consistent environment configuration across tests.
 */
class ParserTestSupport() {
  self: Assertions =>

  private object posToId extends (Pos ~> Id) {
    def apply[A](fa: Pos[A]): Id[A] = fa.value
  }

  /**
   * Parses the input string and returns the full [[ParseResult]].
   */
  def parse(input: String): ParseResult[Pos] = {
    val reader = CharReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    parser.parse(scanner)
  }

  /**
   * Parses the input string and returns the expression with positional information stripped.
   *
   * @throws org.scalatest.exceptions.TestFailedException if there are parse errors or if no value is returned.
   */
  def parseValue(input: String)
                (implicit pos: Position): Expression[Id] = {
    val result = parse(input)
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
   * Parses the input string and returns both the (partial) expression and any errors.
   */
  def parseWithErrors(input: String): (Option[Expression[Id]], Vector[Pos[ParseError]]) = {
    val result = parse(input)
    (result.value.map(_.value.mapK(posToId)), result.errors)
  }
}
