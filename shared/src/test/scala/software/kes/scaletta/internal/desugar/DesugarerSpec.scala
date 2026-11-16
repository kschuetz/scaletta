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

    describe("for-comprehensions (SIP-62)") {
      it("should desugar single generator with simple identifier: for (x <- xs) yield x -> xs.map(x => x)") {
        val xs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val pat = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, pat, xs), 4, 11)
        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 18, 19)), 18, 19)
        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(gen), body), 0, 19)

        val expectedTarget = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val expectedMethod = pos(Identifier("map"), 4, 11)
        val expectedSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expectedTarget, expectedMethod), 9, 11)
        val expectedParam = pos(DesugaredPhase.LambdaParameter(pos(Identifier("x"), 4, 5), None), 4, 5)
        val expectedBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 18, 19)), 18, 19)
        val expectedLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expectedParam), expectedBody), 4, 19)
        val expectedArg = pos(DesugaredPhase.Argument(expectedLambda, None), 4, 19)
        val expectedArgGroup = pos(DesugaredPhase.ArgumentGroup(Vector(expectedArg), None), 4, 19)
        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expectedSelect, Vector.empty, Vector(expectedArgGroup)),
          0,
          19
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar destructuring generator: for ((a, b) <- pairs) yield (a, b)") {
        val pairs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("pairs"), 14, 19)), 14, 19)
        val patA = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("a"), 5, 6)), 5, 6)
        val patB = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("b"), 8, 9)), 8, 9)
        val patTuple = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Tuple(Vector(patA, patB)), 4, 10)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patTuple, pairs), 4, 19)

        val refA = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("a"), 27, 28)), 27, 28)
        val refB = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("b"), 30, 31)), 30, 31)
        val bodyTuple = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Tuple(Vector(refA, refB)), 26, 32)
        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(gen), bodyTuple), 0, 32)

        val expectedTarget = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("pairs"), 14, 19)), 14, 19)
        val expectedMethod = pos(Identifier("map"), 4, 19)
        val expectedSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expectedTarget, expectedMethod), 14, 19)

        val synthId = pos(Identifier("$synth$1"), 4, 10)
        val synthRef = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(synthId), 4, 10)
        val synthParam = pos(DesugaredPhase.LambdaParameter(synthId, None), 4, 10)

        val expPatA = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("a"), 5, 6)), 5, 6)
        val expPatB = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("b"), 8, 9)), 8, 9)
        val expPatTuple = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Tuple(Vector(expPatA, expPatB)), 4, 10)
        val valDecl = pos[DesugaredPhase.Declaration[Pos]](DesugaredPhase.Declaration.Val(expPatTuple, synthRef), 4, 10)

        val expRefA = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("a"), 27, 28)), 27, 28)
        val expRefB = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("b"), 30, 31)), 30, 31)
        val expBodyTuple = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Tuple(Vector(expRefA, expRefB)), 26, 32)

        val lambdaBlock = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Block(Vector(valDecl), expBodyTuple), 4, 32)
        val lambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(synthParam), lambdaBlock), 4, 32)
        val expectedArg = pos(DesugaredPhase.Argument(lambda, None), 4, 32)
        val expectedArgGroup = pos(DesugaredPhase.ArgumentGroup(Vector(expectedArg), None), 4, 32)

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expectedSelect, Vector.empty, Vector(expectedArgGroup)),
          0,
          32
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar value bindings following generator into local val declarations: for (x <- xs; y = x + 41) yield y") {
        val xs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patX, xs), 4, 11)

        val patY = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("y"), 13, 14)), 13, 14)
        val refX = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 17, 18)), 17, 18)
        val lit41 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 21, 23)
        val plusExpr = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Call.Infix(refX, pos(Identifier("+"), 19, 20), Vector.empty, lit41), 17, 23)
        val binding = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.ValueBinding(patY, plusExpr), 13, 23)

        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("y"), 30, 31)), 30, 31)
        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(gen, binding), body), 0, 31)

        val expTarget = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val expMethod = pos(Identifier("map"), 4, 11)
        val expSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTarget, expMethod), 9, 11)
        val expParamX = pos(DesugaredPhase.LambdaParameter(pos(Identifier("x"), 4, 5), None), 4, 5)

        val expPatY = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("y"), 13, 14)), 13, 14)
        val expRefX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 17, 18)), 17, 18)
        val expLit41 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 21, 23)
        val expPlus = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Call.Infix(expRefX, pos(Identifier("+"), 19, 20), Vector.empty, expLit41), 17, 23)
        val expValDecl = pos[DesugaredPhase.Declaration[Pos]](DesugaredPhase.Declaration.Val(expPatY, expPlus), 13, 23)

        val expBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("y"), 30, 31)), 30, 31)
        val expBlock = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Block(Vector(expValDecl), expBody), 13, 31)
        val expLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParamX), expBlock), 4, 31)

        val expArg = pos(DesugaredPhase.Argument(expLambda, None), 4, 31)
        val expArgGroup = pos(DesugaredPhase.ArgumentGroup(Vector(expArg), None), 4, 31)
        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expSelect, Vector.empty, Vector(expArgGroup)),
          0,
          31
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar leading value bindings before generator (better fors): for (x = 41; y <- ys) yield (x, y)") {
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val lit41 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 8, 10)
        val leadingBinding = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.ValueBinding(patX, lit41), 4, 10)

        val ys = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("ys"), 17, 19)), 17, 19)
        val patY = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("y"), 12, 13)), 12, 13)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patY, ys), 12, 19)

        val refX = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 27, 28)), 27, 28)
        val refY = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("y"), 30, 31)), 30, 31)
        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Tuple(Vector(refX, refY)), 26, 32)

        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(leadingBinding, gen), body), 0, 32)

        val expPatX = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val expLit41 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 8, 10)
        val expValDecl = pos[DesugaredPhase.Declaration[Pos]](DesugaredPhase.Declaration.Val(expPatX, expLit41), 4, 10)

        val expTarget = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("ys"), 17, 19)), 17, 19)
        val expMethod = pos(Identifier("map"), 12, 19)
        val expSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTarget, expMethod), 17, 19)
        val expParamY = pos(DesugaredPhase.LambdaParameter(pos(Identifier("y"), 12, 13), None), 12, 13)

        val expRefX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 27, 28)), 27, 28)
        val expRefY = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("y"), 30, 31)), 30, 31)
        val expBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Tuple(Vector(expRefX, expRefY)), 26, 32)
        val expLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParamY), expBody), 12, 32)

        val expArg = pos(DesugaredPhase.Argument(expLambda, None), 12, 32)
        val expArgGroup = pos(DesugaredPhase.ArgumentGroup(Vector(expArg), None), 12, 32)
        val innerCall = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expSelect, Vector.empty, Vector(expArgGroup)),
          12,
          32
        )

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Block(Vector(expValDecl), innerCall),
          0,
          32
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar guards into withFilter chains: for (x <- xs; if x > 41; if x < 47) yield x") {
        val xs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patX, xs), 4, 11)

        val refX1 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 16, 17)), 16, 17)
        val lit41 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 20, 22)
        val cond1 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Call.Infix(refX1, pos(Identifier(">"), 18, 19), Vector.empty, lit41), 16, 22)
        val guard1 = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Guard(cond1), 13, 22)

        val refX2 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 27, 28)), 27, 28)
        val lit47 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(47), 31, 33)
        val cond2 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Call.Infix(refX2, pos(Identifier("<"), 29, 30), Vector.empty, lit47), 27, 33)
        val guard2 = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Guard(cond2), 24, 33)

        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 40, 41)), 40, 41)
        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(gen, guard1, guard2), body), 0, 41)

        val expTarget0 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)

        val wfMethod1 = pos(Identifier("withFilter"), 13, 22)
        val wfSelect1 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTarget0, wfMethod1), 9, 22)
        val expParam1 = pos(DesugaredPhase.LambdaParameter(pos(Identifier("x"), 4, 5), None), 4, 5)
        val expCond1 = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Infix(
            pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 16, 17)), 16, 17),
            pos(Identifier(">"), 18, 19),
            Vector.empty,
            pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 20, 22)
          ),
          16,
          22
        )
        val wfLambda1 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParam1), expCond1), 13, 22)
        val wfArg1 = pos(DesugaredPhase.Argument(wfLambda1, None), 13, 22)
        val wfGroup1 = pos(DesugaredPhase.ArgumentGroup(Vector(wfArg1), None), 13, 22)
        val expTarget1 = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(wfSelect1, Vector.empty, Vector(wfGroup1)),
          9,
          22
        )

        val wfMethod2 = pos(Identifier("withFilter"), 24, 33)
        val wfSelect2 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTarget1, wfMethod2), 9, 33)
        val expParam2 = pos(DesugaredPhase.LambdaParameter(pos(Identifier("x"), 4, 5), None), 4, 5)
        val expCond2 = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Infix(
            pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 27, 28)), 27, 28),
            pos(Identifier("<"), 29, 30),
            Vector.empty,
            pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(47), 31, 33)
          ),
          27,
          33
        )
        val wfLambda2 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParam2), expCond2), 24, 33)
        val wfArg2 = pos(DesugaredPhase.Argument(wfLambda2, None), 24, 33)
        val wfGroup2 = pos(DesugaredPhase.ArgumentGroup(Vector(wfArg2), None), 24, 33)
        val expFinalTarget = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(wfSelect2, Vector.empty, Vector(wfGroup2)),
          9,
          33
        )

        val mapMethod = pos(Identifier("map"), 4, 11)
        val mapSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expFinalTarget, mapMethod), 9, 11)
        val mapParam = pos(DesugaredPhase.LambdaParameter(pos(Identifier("x"), 4, 5), None), 4, 5)
        val mapBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 40, 41)), 40, 41)
        val mapLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(mapParam), mapBody), 4, 41)
        val mapArg = pos(DesugaredPhase.Argument(mapLambda, None), 4, 41)
        val mapGroup = pos(DesugaredPhase.ArgumentGroup(Vector(mapArg), None), 4, 41)

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(mapSelect, Vector.empty, Vector(mapGroup)),
          0,
          41
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar refutable case generators: for (case Some(x) <- xs) yield x") {
        val xs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("xs"), 20, 22)), 20, 22)
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 14, 15)), 14, 15)
        val someTypeId = pos[TypeIdentifier[Pos]](TypeIdentifier.Name(pos(Identifier("Some"), 9, 13)), 9, 13)
        val patSome = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Product(someTypeId, Vector(patX)), 9, 16)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = true, patSome, xs), 4, 22)

        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 29, 30)), 29, 30)
        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(gen), body), 0, 30)

        val expTarget = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("xs"), 20, 22)), 20, 22)

        val wfMethod = pos(Identifier("withFilter"), 4, 22)
        val wfSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTarget, wfMethod), 20, 22)
        val synth1 = pos(Identifier("$synth$1"), 9, 16)
        val synth1Ref = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(synth1), 9, 16)
        val synth1Param = pos(DesugaredPhase.LambdaParameter(synth1, None), 9, 16)

        val expSomeType = pos[TypeIdentifier[Pos]](TypeIdentifier.Name(pos(Identifier("Some"), 9, 13)), 9, 13)
        val expPatX = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("x"), 14, 15)), 14, 15)
        val expPatSome = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Product(expSomeType, Vector(expPatX)), 9, 16)

        val trueLit = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.true_(), 4, 22)
        val falseLit = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.false_(), 4, 22)
        val wildcardPat = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Wildcard(), 4, 22)

        val case1 = pos(DesugaredPhase.Case(expPatSome, None, trueLit), 9, 22)
        val case2 = pos(DesugaredPhase.Case(wildcardPat, None, falseLit), 4, 22)
        val match1 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Match(synth1Ref, Vector(case1, case2)), 4, 22)
        val wfLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(synth1Param), match1), 4, 22)
        val wfArg = pos(DesugaredPhase.Argument(wfLambda, None), 4, 22)
        val wfGroup = pos(DesugaredPhase.ArgumentGroup(Vector(wfArg), None), 4, 22)
        val filteredTarget = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(wfSelect, Vector.empty, Vector(wfGroup)),
          20,
          22
        )

        val mapMethod = pos(Identifier("map"), 4, 22)
        val mapSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(filteredTarget, mapMethod), 20, 22)
        val synth2 = pos(Identifier("$synth$2"), 9, 16)
        val synth2Ref = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(synth2), 9, 16)
        val synth2Param = pos(DesugaredPhase.LambdaParameter(synth2, None), 9, 16)

        val expBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 29, 30)), 29, 30)
        val mapCase = pos(DesugaredPhase.Case(expPatSome, None, expBody), 9, 30)
        val match2 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Match(synth2Ref, Vector(mapCase)), 9, 30)
        val mapLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(synth2Param), match2), 9, 30)
        val mapArg = pos(DesugaredPhase.Argument(mapLambda, None), 9, 30)
        val mapGroup = pos(DesugaredPhase.ArgumentGroup(Vector(mapArg), None), 9, 30)

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(mapSelect, Vector.empty, Vector(mapGroup)),
          0,
          30
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar multiple generators: for (x <- xs; y <- ys) yield (x, y) -> xs.flatMap(x => ys.map(y => (x, y)))") {
        val xs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val genX = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patX, xs), 4, 11)

        val ys = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("ys"), 18, 20)), 18, 20)
        val patY = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("y"), 13, 14)), 13, 14)
        val genY = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patY, ys), 13, 20)

        val refX = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 28, 29)), 28, 29)
        val refY = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("y"), 31, 32)), 31, 32)
        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Tuple(Vector(refX, refY)), 27, 33)

        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(genX, genY), body), 0, 33)

        val expTargetY = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("ys"), 18, 20)), 18, 20)
        val expMethodY = pos(Identifier("map"), 13, 20)
        val expSelectY = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTargetY, expMethodY), 18, 20)
        val expParamY = pos(DesugaredPhase.LambdaParameter(pos(Identifier("y"), 13, 14), None), 13, 14)
        val expRefX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 28, 29)), 28, 29)
        val expRefY = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("y"), 31, 32)), 31, 32)
        val expBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Tuple(Vector(expRefX, expRefY)), 27, 33)
        val expLambdaY = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParamY), expBody), 13, 33)
        val expArgY = pos(DesugaredPhase.Argument(expLambdaY, None), 13, 33)
        val expGroupY = pos(DesugaredPhase.ArgumentGroup(Vector(expArgY), None), 13, 33)
        val expInnerCall = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expSelectY, Vector.empty, Vector(expGroupY)),
          13,
          33
        )

        val expTargetX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val expMethodX = pos(Identifier("flatMap"), 4, 11)
        val expSelectX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTargetX, expMethodX), 9, 11)
        val expParamX = pos(DesugaredPhase.LambdaParameter(pos(Identifier("x"), 4, 5), None), 4, 5)
        val expLambdaX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParamX), expInnerCall), 4, 33)
        val expArgX = pos(DesugaredPhase.Argument(expLambdaX, None), 4, 33)
        val expGroupX = pos(DesugaredPhase.ArgumentGroup(Vector(expArgX), None), 4, 33)

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expSelectX, Vector.empty, Vector(expGroupX)),
          0,
          33
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar multiple leading value bindings and multiple generators: for (x = 41; y = 43; z <- zs; w <- ws) yield (x, y, z, w)") {
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val lit41 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 8, 10)
        val bindX = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.ValueBinding(patX, lit41), 4, 10)

        val patY = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("y"), 12, 13)), 12, 13)
        val lit43 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(43), 16, 18)
        val bindY = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.ValueBinding(patY, lit43), 12, 18)

        val zs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("zs"), 25, 27)), 25, 27)
        val patZ = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("z"), 20, 21)), 20, 21)
        val genZ = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patZ, zs), 20, 27)

        val ws = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("ws"), 34, 36)), 34, 36)
        val patW = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("w"), 29, 30)), 29, 30)
        val genW = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, patW, ws), 29, 36)

        val refX = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 44, 45)), 44, 45)
        val refY = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("y"), 47, 48)), 47, 48)
        val refZ = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("z"), 50, 51)), 50, 51)
        val refW = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("w"), 53, 54)), 53, 54)
        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Tuple(Vector(refX, refY, refZ, refW)), 43, 55)

        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(bindX, bindY, genZ, genW), body), 0, 55)

        val expPatX = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val expLit41 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 8, 10)
        val expDeclX = pos[DesugaredPhase.Declaration[Pos]](DesugaredPhase.Declaration.Val(expPatX, expLit41), 4, 10)

        val expPatY = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("y"), 12, 13)), 12, 13)
        val expLit43 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(43), 16, 18)
        val expDeclY = pos[DesugaredPhase.Declaration[Pos]](DesugaredPhase.Declaration.Val(expPatY, expLit43), 12, 18)

        val expTargetW = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("ws"), 34, 36)), 34, 36)
        val expMethodW = pos(Identifier("map"), 29, 36)
        val expSelectW = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTargetW, expMethodW), 34, 36)
        val expParamW = pos(DesugaredPhase.LambdaParameter(pos(Identifier("w"), 29, 30), None), 29, 30)
        val expRefX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 44, 45)), 44, 45)
        val expRefY = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("y"), 47, 48)), 47, 48)
        val expRefZ = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("z"), 50, 51)), 50, 51)
        val expRefW = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("w"), 53, 54)), 53, 54)
        val expBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Tuple(Vector(expRefX, expRefY, expRefZ, expRefW)), 43, 55)
        val expLambdaW = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParamW), expBody), 29, 55)
        val expArgW = pos(DesugaredPhase.Argument(expLambdaW, None), 29, 55)
        val expGroupW = pos(DesugaredPhase.ArgumentGroup(Vector(expArgW), None), 29, 55)
        val innerCallW = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expSelectW, Vector.empty, Vector(expGroupW)),
          29,
          55
        )

        val expTargetZ = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("zs"), 25, 27)), 25, 27)
        val expMethodZ = pos(Identifier("flatMap"), 20, 27)
        val expSelectZ = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTargetZ, expMethodZ), 25, 27)
        val expParamZ = pos(DesugaredPhase.LambdaParameter(pos(Identifier("z"), 20, 21), None), 20, 21)
        val expLambdaZ = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(expParamZ), innerCallW), 20, 55)
        val expArgZ = pos(DesugaredPhase.Argument(expLambdaZ, None), 20, 55)
        val expGroupZ = pos(DesugaredPhase.ArgumentGroup(Vector(expArgZ), None), 20, 55)
        val innerCallZ = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(expSelectZ, Vector.empty, Vector(expGroupZ)),
          20,
          55
        )

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Block(Vector(expDeclX, expDeclY), innerCallZ),
          0,
          55
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar comprehension with only value bindings: for (x = 41; y = 43) yield (x, y)") {
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val lit41 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 8, 10)
        val bindX = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.ValueBinding(patX, lit41), 4, 10)

        val patY = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("y"), 12, 13)), 12, 13)
        val lit43 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(43), 16, 18)
        val bindY = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.ValueBinding(patY, lit43), 12, 18)

        val refX = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 26, 27)), 26, 27)
        val refY = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("y"), 29, 30)), 29, 30)
        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Tuple(Vector(refX, refY)), 25, 31)

        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(bindX, bindY), body), 0, 31)

        val expPatX = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val expLit41 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 8, 10)
        val expDeclX = pos[DesugaredPhase.Declaration[Pos]](DesugaredPhase.Declaration.Val(expPatX, expLit41), 4, 10)

        val expPatY = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("y"), 12, 13)), 12, 13)
        val expLit43 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(43), 16, 18)
        val expDeclY = pos[DesugaredPhase.Declaration[Pos]](DesugaredPhase.Declaration.Val(expPatY, expLit43), 12, 18)

        val expRefX = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 26, 27)), 26, 27)
        val expRefY = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("y"), 29, 30)), 29, 30)
        val expBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Tuple(Vector(expRefX, expRefY)), 25, 31)

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Block(Vector(expDeclX, expDeclY), expBody),
          0,
          31
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should desugar refutable case generator with guard: for (case Some(x) <- xs; if x > 41) yield x") {
        val xs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("xs"), 20, 22)), 20, 22)
        val patX = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 14, 15)), 14, 15)
        val someTypeId = pos[TypeIdentifier[Pos]](TypeIdentifier.Name(pos(Identifier("Some"), 9, 13)), 9, 13)
        val patSome = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Product(someTypeId, Vector(patX)), 9, 16)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = true, patSome, xs), 4, 22)

        val refX = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 27, 28)), 27, 28)
        val lit41 = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Literal.int(41), 31, 33)
        val cond = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Call.Infix(refX, pos(Identifier(">"), 29, 30), Vector.empty, lit41), 27, 33)
        val guard = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Guard(cond), 24, 33)

        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 40, 41)), 40, 41)
        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(gen, guard), body), 0, 41)

        val expTarget = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("xs"), 20, 22)), 20, 22)

        val wfMethod = pos(Identifier("withFilter"), 24, 33)
        val wfSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(expTarget, wfMethod), 20, 33)
        val synth1 = pos(Identifier("$synth$1"), 9, 16)
        val synth1Ref = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(synth1), 9, 16)
        val synth1Param = pos(DesugaredPhase.LambdaParameter(synth1, None), 9, 16)

        val expSomeType = pos[TypeIdentifier[Pos]](TypeIdentifier.Name(pos(Identifier("Some"), 9, 13)), 9, 13)
        val expPatX = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier(pos(Identifier("x"), 14, 15)), 14, 15)
        val expPatSome = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Product(expSomeType, Vector(expPatX)), 9, 16)

        val expCond = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Infix(
            pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 27, 28)), 27, 28),
            pos(Identifier(">"), 29, 30),
            Vector.empty,
            pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int(41), 31, 33)
          ),
          27,
          33
        )

        val trueLit = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.true_(), 24, 33)
        val falseLit = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.false_(), 24, 33)
        val wildcardPat = pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Wildcard(), 24, 33)

        val case1 = pos(DesugaredPhase.Case(expPatSome, Some(expCond), trueLit), 9, 33)
        val case2 = pos(DesugaredPhase.Case(wildcardPat, None, falseLit), 24, 33)
        val match1 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Match(synth1Ref, Vector(case1, case2)), 24, 33)
        val wfLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(synth1Param), match1), 24, 33)
        val wfArg = pos(DesugaredPhase.Argument(wfLambda, None), 24, 33)
        val wfGroup = pos(DesugaredPhase.ArgumentGroup(Vector(wfArg), None), 24, 33)
        val filteredTarget = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(wfSelect, Vector.empty, Vector(wfGroup)),
          20,
          33
        )

        val mapMethod = pos(Identifier("map"), 4, 22)
        val mapSelect = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Select(filteredTarget, mapMethod), 20, 22)
        val synth2 = pos(Identifier("$synth$2"), 9, 16)
        val synth2Ref = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(synth2), 9, 16)
        val synth2Param = pos(DesugaredPhase.LambdaParameter(synth2, None), 9, 16)

        val expBody = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference(pos(Identifier("x"), 40, 41)), 40, 41)
        val mapCase = pos(DesugaredPhase.Case(expPatSome, None, expBody), 9, 41)
        val match2 = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Match(synth2Ref, Vector(mapCase)), 9, 41)
        val mapLambda = pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda(Vector(synth2Param), match2), 9, 41)
        val mapArg = pos(DesugaredPhase.Argument(mapLambda, None), 9, 41)
        val mapGroup = pos(DesugaredPhase.ArgumentGroup(Vector(mapArg), None), 9, 41)

        val expected = pos[DesugaredPhase.Expression[Pos]](
          DesugaredPhase.Call.Standard(mapSelect, Vector.empty, Vector(mapGroup)),
          0,
          41
        )

        desugarer.desugar(forYield) shouldBe expected
      }

      it("should assert non-empty valid Pos across synthetic nodes") {
        val xs = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("xs"), 9, 11)), 9, 11)
        val pat = pos[ParsedPhase.Pattern[Pos]](ParsedPhase.Pattern.Identifier(pos(Identifier("x"), 4, 5)), 4, 5)
        val gen = pos[ParsedPhase.Enumerator[Pos]](ParsedPhase.Enumerator.Generator(isCase = false, pat, xs), 4, 11)
        val body = pos[ParsedPhase.Expression[Pos]](ParsedPhase.Reference(pos(Identifier("x"), 18, 19)), 18, 19)
        val forYield = pos[ParsedPhase.Expression[Pos]](ParsedPhase.ForYield(Vector(gen), body), 0, 19)

        val result = desugarer.desugar(forYield)
        result.begin.value should be <= result.end.value
        (result.value match {
          case _: DesugaredPhase.Call.Standard[Pos] @unchecked => true
          case _ => false
        }) shouldBe true
        val call = result.value.asInstanceOf[DesugaredPhase.Call.Standard[Pos]]
        call.target.begin.value should be <= call.target.end.value
        call.args.head.value.arguments.head.value.value.begin.value should be <= call.args.head.value.arguments.head.value.value.end.value
      }
    }
  }
}
