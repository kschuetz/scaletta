package software.kes.scaletta.testsupport

import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.Token

object TestErrorFormatting {

  def renderUnderline(input: String, index: Int, label: String): String = {
    val lines = input.split("\n")
    var currentIdx = 0
    val result = new StringBuilder()

    // Find the line containing the error index
    val errorLineIdx = lines.indexWhere { line =>
      val start = currentIdx
      val end = currentIdx + line.length
      currentIdx += line.length + 1 // +1 for newline
      index >= start && index <= end
    }

    if (errorLineIdx != -1) {
      // Recalculate start index of the target line to find column
      val lineStartIdx = lines.take(errorLineIdx).map(_.length + 1).sum
      val col = index - lineStartIdx

      // Show context: previous, current, and next line
      val startLine = Math.max(0, errorLineIdx - 1)
      val endLine = Math.min(lines.length - 1, errorLineIdx + 1)

      for (i <- startLine to endLine) {
        val lineNum = i + 1
        result.append(f"$lineNum%4d | ${lines(i)}\n")
        if (i == errorLineIdx) {
          val padding = " " * (col + 7) // 4 (fmt) + 3 (separator)
          result.append(padding).append("^--- ").append(label).append("\n")
        }
      }
    }
    result.toString()
  }

  def formatToken(p: Pos[Token]): String = s"${p.value} at ${p.begin}:${p.end}"
}
