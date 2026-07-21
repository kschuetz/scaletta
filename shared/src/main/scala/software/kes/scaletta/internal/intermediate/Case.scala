package software.kes.scaletta.internal.intermediate

case class Case(pattern: Pattern,
                guard: Option[IntermediateExpression],
                body: IntermediateExpression)
