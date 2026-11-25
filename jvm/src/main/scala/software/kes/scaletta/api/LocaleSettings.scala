package software.kes.scaletta.api

import java.util.Locale

object LocaleSettings {
  val Default: LocaleSettings = LocaleSettings()
}

case class LocaleSettings(locale: Locale = Locale.ROOT)
