package software.kes.scaletta.testsupport

import software.kes.scaletta.scanner.{CharReader, LineMap}

object TestReaderFactory {
  def fromString[A](s: String)
                   (body: CharReader => A): A = {
    val lineMap = LineMap.create()
    val reader = CharReader.create(s.iterator, lineMap.builder)
    body(reader)
  }
}
