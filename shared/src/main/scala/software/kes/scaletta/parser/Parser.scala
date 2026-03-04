package software.kes.scaletta.parser

import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.Scanner

final class Parser {
  def parse(scanner: Scanner): ParseResult[Pos] = {
    ParseResult.empty[Pos]
  }
}
