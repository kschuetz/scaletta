package software.kes.scaletta.internal.desugar

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}

final class DesugarerSpec extends AnyFunSpec with Matchers {

  private val desugarer = Desugarer.create()

  private def pos[A](value: A, begin: Int, end: Int): Pos[A] =
    Pos(value, CharIndex(begin), CharIndex(end))

  describe("Desugarer") {
    it("should desugar literals preserving values and positions") {
      val intLit = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 0, 2)
      desugarer.desugar(intLit) shouldBe pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 0, 2)

      val strLit = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.string("hello"), 0, 7)
      desugarer.desugar(strLit) shouldBe pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.string("hello"), 0, 7)

      val boolLit = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.true_(), 0, 4)
      desugarer.desugar(boolLit) shouldBe pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.true_(), 0, 4)

      val nullLit = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.null_(), 0, 4)
      desugarer.desugar(nullLit) shouldBe pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.null_(), 0, 4)

      val unitLit = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.unit(), 0, 2)
      desugarer.desugar(unitLit) shouldBe pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.unit(), 0, 2)
    }

    it("should desugar references and selects") {
      val ref = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("a"), 0, 1)), 0, 1)
      desugarer.desugar(ref) shouldBe pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("a"), 0, 1)), 0, 1)

      val select = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Select(ref, pos(Identifier("b"), 2, 3)), 0, 3)
      desugarer.desugar(select) shouldBe pos[DesugaredPhase.Expression[Pos]](
        DesugaredPhase.Select(
          pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("a"), 0, 1)), 0, 1),
          pos(Identifier("b"), 2, 3)
        ),
        0,
        3
      )
    }

    it("should desugar calls (infix and standard)") {
      val left = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 0, 2)
      val right = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(43), 5, 7)
      val infix = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Call.Infix(left, pos(Identifier("+"), 3, 4), Vector.empty, right), 0, 7)

      desugarer.desugar(infix) shouldBe pos[DesugaredPhase.Expression[Pos]](
        DesugaredPhase.Call.Infix(
          pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 0, 2),
          pos(Identifier("+"), 3, 4),
          Vector.empty,
          pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(43), 5, 7)
        ),
        0,
        7
      )

      val target = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("f"), 0, 1)), 0, 1)
      val arg = pos(ParsedPhase.Argument(pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 2, 4), None), 2, 4)
      val group = pos(ParsedPhase.ArgumentGroup(Vector(arg)), 1, 5)
      val standardCall = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Call.Standard(target, Vector.empty, Vector(group)), 0, 5)

      desugarer.desugar(standardCall) shouldBe pos[DesugaredPhase.Expression[Pos]](
        DesugaredPhase.Call.Standard(
          pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("f"), 0, 1)), 0, 1),
          Vector.empty,
          Vector(
            pos(
              DesugaredPhase.ArgumentGroup(
                Vector(pos(DesugaredPhase.Argument(pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 2, 4), None), 2, 4))
              ),
              1,
              5
            )
          )
        ),
        0,
        5
      )
    }

    it("should desugar lambdas") {
      val param = pos(ParsedPhase.LambdaParameter(pos(Identifier("x"), 1, 2), None), 1, 2)
      val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 6, 7)), 6, 7)
      val lambda = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Lambda(Vector(param), body), 0, 7)

      desugarer.desugar(lambda) shouldBe pos[DesugaredPhase.Expression[Pos]](
        DesugaredPhase.Lambda(
          Vector(pos(DesugaredPhase.LambdaParameter(pos(Identifier("x"), 1, 2), None), 1, 2)),
          pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 6, 7)), 6, 7)
        ),
        0,
        7
      )
    }

    it("should desugar blocks and declarations") {
      val pat = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
      val rhs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 8, 10)
      val valDecl = pos[ParsedPhase.Declaration[Pos]](ParsedPhase.Declaration.Val(pat, rhs), 0, 10)
      val res = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 12, 13)), 12, 13)
      val block = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Block(Vector(valDecl), res), 0, 14)

      desugarer.desugar(block) shouldBe pos[DesugaredPhase.Expression[Pos]](
        DesugaredPhase.Block(
          Vector(
            pos[DesugaredPhase.Declaration[Pos]](
              DesugaredPhase.Declaration.Val(
                pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5),
                pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 8, 10)
              ),
              0,
              10
            )
          ),
          pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 12, 13)), 12, 13)
        ),
        0,
        14
      )
    }

    it("should desugar match expressions and cases") {
      val scrutinee = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 0, 2)
      val pat = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 10, 11)), 10, 11)
      val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 15, 16)), 15, 16)
      val c = pos(ParsedPhase.Case(pat, None, body), 10, 16)
      val matchExpr = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Match(scrutinee, Vector(c)), 0, 18)

      desugarer.desugar(matchExpr) shouldBe pos[DesugaredPhase.Expression[Pos]](
        DesugaredPhase.Match(
          pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 0, 2),
          Vector(
            pos(
              DesugaredPhase.Case(
                pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("x"), 10, 11)), 10, 11),
                None,
                pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 15, 16)), 15, 16)
              ),
              10,
              16
            )
          )
        ),
        0,
        18
      )
    }
  }
}
