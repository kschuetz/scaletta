package software.kes.scaletta.api

case class FormalParameter(name: Name,
                           typ: Type[TypeId],
                           default: Option[Any] = None)
