package software.kes.scaletta.testsupport

import software.kes.scaletta.scanner.{Pos, Token}

object TestErrorFormatting {

  def renderUnderline(input: String, index: Int, label: String): String = {
    val lines = input.split("\n")
    var currentIdx = 0
    val result = new StringBuilder()
    var found = false
    lines.foreach { line =>
      if (!found && index >= currentIdx && index <= currentIdx + line.length) {
        result.append(line).append("\n")
        val padding = " " * (index - currentIdx)
        result.append(padding).append("^--- ").append(label).append("\n")
        found = true
      }
      currentIdx += line.length + 1 // +1 for newline
    }
    result.toString()
  }

  def formatToken(p: Pos[Token]): String = s"${p.value} at ${p.begin}:${p.end}"
}
