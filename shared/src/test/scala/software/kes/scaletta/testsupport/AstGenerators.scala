package software.kes.scaletta.testsupport

import org.scalacheck.{Arbitrary, Gen}
import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.util.functional.Id._

object AstGenerators {

  def genIdentifier: Gen[String] = Gen.oneOf("a", "b", "c", "x", "y", "z", "f", "g", "h", "myVar",
    "anotherVar", "result")

  def genTypeIdentifier: Gen[String] = Gen.oneOf("Int", "String", "Boolean", "Double", "Long",
    "Any", "Nothing", "Unit")

  def genLiteral: Gen[Literal[Id]] = Gen.oneOf(
    Gen.choose(-100, 100).map(Literal.int[Id]),
    Gen.choose(-100L, 100L).map(Literal.long[Id]),
    Gen.choose(-100.0f, 100.0f).map(Literal.float[Id]),
    Gen.choose(-100.0, 100.0).map(Literal.double[Id]),
    Gen.oneOf(true, false).map(Literal.boolean[Id]),
    Gen.const(Literal.null_[Id]()),
    Gen.alphaChar.map(Literal.char[Id]),
    Gen.alphaStr.map(Literal.string[Id])
  )

  def genReference: Gen[Reference[Id]] = for {
    parts <- Gen.nonEmptyListOf(genIdentifier)
  } yield {
    val idents = parts.map(Identifier[Id](_))
    Reference[Id](::(idents.head, idents.tail))
  }

  def genPattern(depth: Int): Gen[Pattern[Id]] = {
    if (depth <= 0) {
      Gen.oneOf(
        Gen.const(Pattern.Wildcard[Id]()),
        genIdentifier.map(n => Pattern.Identifier[Id](Identifier[Id](n))),
        genLiteral.map(l => Pattern.Literal[Id](l))
      )
    } else {
      Gen.frequency(
        (3, Gen.const(Pattern.Wildcard[Id]())),
        (3, genIdentifier.map(n => Pattern.Identifier[Id](Identifier[Id](n)))),
        (2, genLiteral.map(l => Pattern.Literal[Id](l))),
        (1, for {
          name <- genIdentifier
          p <- genPattern(depth - 1)
        } yield Pattern.As[Id](Identifier[Id](name), p)),
        (1, for {
          p <- genPattern(depth - 1)
          t <- genTypeIdentifier
        } yield Pattern.Typed[Id](p, TypeIdentifier.name(Identifier[Pos](t)))),
        (1, for {
          n <- Gen.choose(2, 4)
          elements <- Gen.listOfN(n, genPattern(depth - 1))
        } yield Pattern.Tuple[Id](elements.toVector)),
        (1, for {
          t <- genTypeIdentifier
          n <- Gen.choose(0, 3)
          args <- Gen.listOfN(n, genPattern(depth - 1))
        } yield Pattern.Product[Id](TypeIdentifier.name(Identifier[Pos](t)), args.toVector))
      )
    }
  }

  def genExpression(depth: Int): Gen[Expression[Id]] = {
    if (depth <= 0) {
      Gen.oneOf(genLiteral, genReference)
    } else {
      Gen.frequency(
        (5, genLiteral),
        (5, genReference),
        (2, for {
          n <- Gen.choose(2, 4)
          elements <- Gen.listOfN(n, genExpression(depth - 1))
        } yield Tuple[Id](elements.toVector)),
        (2, for {
          target <- genExpression(depth - 1)
          nGroups <- Gen.choose(1, 2)
          groups <- Gen.listOfN(nGroups, for {
            nArgs <- Gen.choose(0, 3)
            args <- Gen.listOfN(nArgs, genExpression(depth - 1))
          } yield ArgumentGroup[Id](args.toVector.map(a => Argument[Id](a))))
        } yield Call.standard[Id](target, Vector.empty, groups.toVector)),
        (2, for {
          left <- genExpression(depth - 1)
          op <- Gen.oneOf("+", "-", "*", "/", "==", "!=", "<", ">", "&&", "||")
          right <- genExpression(depth - 1)
        } yield Call.infix[Id](left, Identifier[Id](op), Vector.empty, right)),
        (1, for {
          cond <- genExpression(depth - 1)
          thenB <- genExpression(depth - 1)
          elseB <- genExpression(depth - 1)
        } yield Conditional[Id](cond, thenB, elseB)),
        (1, for {
          nDecls <- Gen.choose(1, 3)
          decls <- Gen.listOfN(nDecls, genDeclaration(depth - 1))
          res <- genExpression(depth - 1)
        } yield Block[Id](decls.toVector, res)),
        (1, for {
          expr <- genExpression(depth - 1)
          nCases <- Gen.choose(1, 3)
          cases <- Gen.listOfN(nCases, for {
            p <- genPattern(depth - 1)
            body <- genExpression(depth - 1)
          } yield Case[Id](p, None, body))
          // Always add a wildcard case to ensure pseudo-exhaustiveness
          wildcardCase = Case[Id](Pattern.Wildcard[Id](), None, Literal.int[Id](0))
        } yield Match[Id](expr, (cases :+ wildcardCase).toVector))
      )
    }
  }

  def genDeclaration(depth: Int): Gen[Declaration[Id]] = {
    Gen.oneOf(
      for {
        p <- genPattern(depth)
        rhs <- genExpression(depth)
      } yield Declaration.val_[Id](p, rhs),
      for {
        p <- genPattern(depth)
        rhs <- genExpression(depth)
      } yield Declaration.lazyVal[Id](p, rhs),
      for {
        name <- genIdentifier
        nParams <- Gen.choose(0, 2)
        params <- Gen.listOfN(nParams, for {
          nP <- Gen.choose(1, 3)
          group <- Gen.listOfN(nP, for {
            pName <- genIdentifier
            pType <- genTypeIdentifier
          } yield FormalParameter[Id](Identifier[Id](pName), TypeIdentifier.name(Identifier[Pos](pType)), None))
        } yield FormalParameterGroup[Id](group.toVector))
        body <- genExpression(depth)
      } yield Declaration.def_[Id](Identifier[Id](name), params.toVector, body)
    )
  }

  implicit val arbExpression: Arbitrary[Expression[Id]] = Arbitrary(genExpression(3))
}
