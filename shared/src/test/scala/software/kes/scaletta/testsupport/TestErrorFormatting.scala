package software.kes.scaletta.testsupport

import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.Token

object TestErrorFormatting {

  def renderUnderline(input: String, lineMap: software.kes.scaletta.reporting.LineMap, index: Int, label: String): String = {
    val pos = lineMap.indexToPosition(software.kes.scaletta.reporting.CharIndex(index))
    val lines = input.split("\n")
    val result = new StringBuilder()

    val errorLineIdx = pos.line.value
    val col = pos.column.value

    if (errorLineIdx >= 0 && errorLineIdx < lines.length) {
      val fullLabel = s"($pos) $label"

      // Show context: previous, current, and next line
      val startLine = Math.max(0, errorLineIdx - 1)
      val endLine = Math.min(lines.length - 1, errorLineIdx + 1)

      for (i <- startLine to endLine) {
        val lineNum = i + 1
        result.append(f"$lineNum%4d | ${lines(i)}\n")
        if (i == errorLineIdx) {
          val padding = " " * (col + 7) // 4 (fmt) + 3 (separator)
          result.append(padding).append("^--- ").append(fullLabel).append("\n")
        }
      }
    }
    result.toString()
  }

  def formatToken(p: Pos[Token]): String = s"${p.value} at ${p.begin}:${p.end}"
}
