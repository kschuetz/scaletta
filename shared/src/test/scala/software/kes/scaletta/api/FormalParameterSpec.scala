package software.kes.scaletta.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.dsl._

class FormalParameterSpec extends AnyFunSuite with Matchers {
  test("FormalParameter.apply should allow String for name") {
    val typ = Type.Nominal(TypeId(41))
    val p = FormalParameter("x", typ)
    p.name.value shouldBe "x"
    p.typ shouldBe typ
    p.default shouldBe None
  }

  test("FormalParameter.apply should allow default value") {
    val typ = Type.Nominal(TypeId(43))
    val p = FormalParameter("y", typ, Some(100))
    p.name.value shouldBe "y"
    p.typ shouldBe typ
    p.default shouldBe Some(100)
  }

  test("DSL should allow :> operator for FormalParameter construction") {
    import software.kes.scaletta.api.dsl._
    val typ = Type.Nominal(TypeId(47))
    val p = "z" ofType typ
    p.name.value shouldBe "z"
    p.typ shouldBe typ
    p.default shouldBe None
  }

  test("DSL should allow implicit conversion from String to Name") {
    import software.kes.scaletta.api.dsl._
    val name: Name = "foo"
    name.value shouldBe "foo"
  }
}
