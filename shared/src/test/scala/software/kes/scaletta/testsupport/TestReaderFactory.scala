package software.kes.scaletta.testsupport

import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.reporting.LineMap

object TestReaderFactory {
  def fromString[A](s: String,
                    settings: SourceReader.Settings = SourceReader.Settings())
                   (body: (SourceReader, LineMap) => A): A = {
    val lineMap = LineMap.create()
    val reader = SourceReader.create(s.iterator, lineMap.builder, settings = settings)
    body(reader, lineMap)
  }
}
