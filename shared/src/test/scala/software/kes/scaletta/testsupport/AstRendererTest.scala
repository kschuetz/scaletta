package software.kes.scaletta.testsupport

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast._
import software.kes.scaletta.util.functional.Id.Id

class AstRendererTest extends AnyFunSuite with Matchers {

  test("render simple literal") {
    AstRenderer.render(Literal.int(1)) shouldBe "1"
    AstRenderer.render(Literal.string("hello")) shouldBe "\"hello\""
    AstRenderer.render(Literal.boolean(true)) shouldBe "true"
  }

  test("render reference") {
    val ref = Reference[Id](::(Identifier("foo"), List(Identifier("bar"))))
    AstRenderer.render(ref) shouldBe "foo.bar"
  }

  test("render tuple") {
    val tuple = Tuple[Id](Vector(Literal.int(1), Literal.string("a")))
    AstRenderer.render(tuple) shouldBe "(1, \"a\")"
  }

  test("render conditional") {
    val cond = Conditional[Id](Literal.boolean(true), Literal.int(1), Literal.int(2))
    AstRenderer.render(cond) shouldBe "if (true) 1 else 2"
  }

  test("render block") {
    val block = Block[Id](Vector.empty, Literal.int(1))
    AstRenderer.render(block) shouldBe "{\n  1\n}"

    val blockWithDecls = Block[Id](
      Vector(Declaration.val_[Id](Pattern.Identifier[Id](Identifier("x")), Literal.int(1))),
      Reference.single[Id](Identifier("x"))
    )
    AstRenderer.render(blockWithDecls) shouldBe "{\n  val x = 1\n  x\n}"
  }

  test("render settings: compact mode") {
    val settings = AstRenderer.Settings(compact = true)
    val block = Block[Id](
      Vector(Declaration.val_[Id](Pattern.Identifier[Id](Identifier("x")), Literal.int(1))),
      Reference.single[Id](Identifier("x"))
    )
    AstRenderer.render(block, settings) shouldBe "{ val x = 1 x }"

    val m = Match[Id](
      Literal.int(1),
      Vector(
        Case[Id](Pattern.Literal[Id](Literal.int(1)), None, Literal.string("one")),
        Case[Id](Pattern.Wildcard[Id](), None, Literal.string("other"))
      )
    )
    AstRenderer.render(m, settings) shouldBe "1 match { case 1 => \"one\" case _ => \"other\" }"
  }

  test("render settings: indent size") {
    val settings = AstRenderer.Settings(indentSize = 4)
    val block = Block[Id](
      Vector(Declaration.val_[Id](Pattern.Identifier[Id](Identifier("x")), Literal.int(1))),
      Reference.single[Id](Identifier("x"))
    )
    AstRenderer.render(block, settings) shouldBe "{\n    val x = 1\n    x\n}"
  }

  test("render settings: parenthesize all calls") {
    val settings = AstRenderer.Settings(parenthesizeAllCalls = true)
    val call = Call.standard[Id](
      Reference.single[Id](Identifier("foo")),
      Vector.empty,
      Vector(ArgumentGroup[Id](Vector(Argument[Id](Literal.int(1)))))
    )
    AstRenderer.render(call, settings) shouldBe "(foo)(1)"
  }

  test("render infix call with precedence") {
    val plus = Identifier("+")
    val times = Identifier("*")

    // (1 + 2) * 3
    val expr1 = Call.infix[Id](
      Call.infix[Id](Literal.int(1), plus, Vector.empty, Literal.int(2)),
      times,
      Vector.empty,
      Literal.int(3)
    )
    AstRenderer.render(expr1) shouldBe "(1 + 2) * 3"

    // 1 + 2 * 3
    val expr2 = Call.infix[Id](
      Literal.int(1),
      plus,
      Vector.empty,
      Call.infix[Id](Literal.int(2), times, Vector.empty, Literal.int(3))
    )
    AstRenderer.render(expr2) shouldBe "1 + 2 * 3"
  }

  test("render lambda") {
    val lambda = Lambda[Id](
      Vector(LambdaParameter[Id](Identifier("x"), Some(TypeIdentifier.name(Identifier("Int"))))),
      Call.infix[Id](Reference.single[Id](Identifier("x")), Identifier("+"), Vector.empty, Literal.int(1))
    )
    AstRenderer.render(lambda) shouldBe "(x: Int) => x + 1"
  }

  test("render match") {
    val m = Match[Id](
      Literal.int(1),
      Vector(
        Case[Id](Pattern.Literal[Id](Literal.int(1)), None, Literal.string("one")),
        Case[Id](Pattern.Wildcard[Id](), None, Literal.string("other"))
      )
    )
    AstRenderer.render(m) shouldBe "1 match {\n  case 1 => \"one\"\n  case _ => \"other\"\n}"
  }

  test("render conjunction types") {
    val typeA = TypeIdentifier.name(Identifier("A"))
    val typeB = TypeIdentifier.name(Identifier("B"))
    val typeC = TypeIdentifier.name(Identifier("C"))
    val union = TypeIdentifier.union(typeA, typeB)

    // Simple union
    AstRenderer.render(Typed[Id](Literal.int(1), union)) shouldBe "(1: A | B)"

    // Nested union/intersection (should be parenthesized by renderType)
    val intersection = TypeIdentifier.intersection(union, typeC)
    // Note: Conjunction flattening might occur depending on implementation, 
    // but TypeIdentifier.intersection(union, typeC) with different types won't flatten across Union/Intersection.
    AstRenderer.render(Typed[Id](Literal.int(1), intersection)) shouldBe "(1: (A | B) & C)"

    // Function type in conjunction
    val funcType = TypeIdentifier.function(Vector(typeA), typeB)
    val combined = TypeIdentifier.union(funcType, typeC)
    AstRenderer.render(Typed[Id](Literal.int(1), combined)) shouldBe "(1: (A => B) | C)"
  }
}
