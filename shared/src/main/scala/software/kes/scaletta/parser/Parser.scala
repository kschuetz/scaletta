package software.kes.scaletta.parser

import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.Scanner

object Parser {
  def create(): Parser = new Parser()
}

final class Parser private() {
  def parse(scanner: Scanner): ParseResult[Pos] = {
    ParseResult.empty[Pos]
  }
}
