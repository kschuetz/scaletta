package software.kes.scaletta.testsupport

import software.kes.scaletta.reporting.LineMap
import software.kes.scaletta.scanner.CharReader

object TestReaderFactory {
  def fromString[A](s: String,
                    settings: CharReader.Settings = CharReader.Settings())
                   (body: CharReader => A): A = {
    val lineMap = LineMap.create()
    val reader = CharReader.create(s.iterator, lineMap.builder, settings = settings)
    body(reader)
  }
}
