package software.kes.scaletta.api

import software.kes.scaletta.internal.symbols.Name

case class MethodName(receiverType: ReceiverType,
                      name: Name)
