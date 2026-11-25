package software.kes.scaletta.internal.interpreter.testsupport

import software.kes.scaletta.internal.format.ValueFormatter

abstract class PlatformTestInterpreterFactory {
  protected lazy val valueFormatter: ValueFormatter =
    ValueFormatter.create()
}
