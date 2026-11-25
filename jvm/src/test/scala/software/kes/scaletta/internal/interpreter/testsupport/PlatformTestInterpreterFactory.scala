package software.kes.scaletta.internal.interpreter.testsupport

import software.kes.scaletta.internal.format.ValueFormatter

import java.util.Locale

abstract class PlatformTestInterpreterFactory {
  protected lazy val valueFormatter: ValueFormatter =
    ValueFormatter.create(Locale.ROOT)
}
