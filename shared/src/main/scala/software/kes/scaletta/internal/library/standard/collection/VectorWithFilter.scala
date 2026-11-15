package software.kes.scaletta.internal.library.standard.collection

import software.kes.scaletta.api.CallTarget

case class VectorWithFilter(underlying: Vector[Any],
                            predicates: Vector[CallTarget])
