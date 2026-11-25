package software.kes.scaletta.api

import software.kes.scaletta.internal.format.RuntimeFormattingError

abstract class ScalettaRuntimeException(message: String) extends RuntimeException(message)

final class RuntimeFormattingException(message: String,
                                       errorDetails: RuntimeFormattingError) extends ScalettaRuntimeException(message)
