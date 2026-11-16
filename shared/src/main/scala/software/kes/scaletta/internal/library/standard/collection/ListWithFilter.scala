package software.kes.scaletta.internal.library.standard.collection

import software.kes.scaletta.api.CallTarget

case class ListWithFilter(underlying: List[Any],
                          predicates: Vector[CallTarget])
