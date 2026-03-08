package software.kes.scaletta.testsupport

import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.Token

object TestErrorFormatting {

  def renderUnderline(input: String, lineMap: software.kes.scaletta.reporting.LineMap, beginIndex: Int, endIndex: Option[Int], label: String): String = {
    val beginPos = lineMap.indexToPosition(software.kes.scaletta.reporting.CharIndex(beginIndex))
    val lines = input.split("\n")
    val result = new StringBuilder()

    val errorLineIdx = beginPos.line.value - 1
    val col = beginPos.column.value - 1

    if (errorLineIdx >= 0 && errorLineIdx < lines.length) {
      val fullLabel = s"($beginPos) $label"

      // Show context: previous, current, and next line
      val startLine = Math.max(0, errorLineIdx - 1)
      val endLine = Math.min(lines.length - 1, errorLineIdx + 1)

      for (i <- startLine to endLine) {
        val lineNum = i + 1
        result.append(f"$lineNum%4d | ${lines(i)}\n")
        if (i == errorLineIdx) {
          val padding = " " * (col + 7) // 4 (fmt) + 3 (separator)
          val length = endIndex.map(_ - beginIndex + 1).getOrElse(1)
          val underline = "^" + ("~" * Math.max(0, length - 1))
          result.append(padding).append(underline).append("--- ").append(fullLabel).append("\n")
        }
      }
    }
    result.toString()
  }

  def renderUnderline(input: String, lineMap: software.kes.scaletta.reporting.LineMap, index: Int, label: String): String =
    renderUnderline(input, lineMap, index, None, label)

  def formatToken(p: Pos[Token]): String = s"${p.value} at ${p.begin}:${p.end}"
}
