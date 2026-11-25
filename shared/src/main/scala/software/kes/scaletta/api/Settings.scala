package software.kes.scaletta.api

import software.kes.scaletta.internal.scanner.IdentifierPolicy

case class Settings(identifierPolicy: IdentifierPolicy = IdentifierPolicy.Default,
                    localeSettings: LocaleSettings = LocaleSettings.Default,
                    stringFormatterErrorHandler: StringFormatterErrorHandler = StringFormatterErrorHandler.default)
