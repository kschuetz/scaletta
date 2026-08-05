package software.kes.scaletta.internal

import software.kes.scaletta.internal.ast.ParsingPhase.{Declaration, Expression, Pattern}
import software.kes.scaletta.internal.ast.TypeIdentifier

package object parser {
  type ExprResult[F[_]] = ParseResult[F, F[Expression[F]]]
  type DeclResult[F[_]] = ParseResult[F, F[Declaration[F]]]
  type PatResult[F[_]] = ParseResult[F, F[Pattern[F]]]
  type TypeResult[F[_]] = ParseResult[F, F[TypeIdentifier[F]]]
}
