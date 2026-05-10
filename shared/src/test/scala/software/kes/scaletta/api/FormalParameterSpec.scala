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

  test("DSL should allow ofType operator for FormalParameter construction") {
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

  test("FormalParameter should provide constructors for primitive types") {
    import software.kes.scaletta.internal.runtime.CoreTypes

    FormalParameter.boolean("p2", Some(true)) shouldBe FormalParameter("p2", CoreTypes.BooleanT, Some(true))
    FormalParameter.byte("p3", Some(1.toByte)) shouldBe FormalParameter("p3", CoreTypes.ByteT, Some(1.toByte))
    FormalParameter.char("p4", Some('a')) shouldBe FormalParameter("p4", CoreTypes.CharT, Some('a'))
    FormalParameter.double("p5", Some(3.14)) shouldBe FormalParameter("p5", CoreTypes.DoubleT, Some(3.14))
    FormalParameter.float("p6", Some(1.23f)) shouldBe FormalParameter("p6", CoreTypes.FloatT, Some(1.23f))
    FormalParameter.int("p7", Some(41)) shouldBe FormalParameter("p7", CoreTypes.IntT, Some(41))
    FormalParameter.long("p8", Some(123L)) shouldBe FormalParameter("p8", CoreTypes.LongT, Some(123L))
    FormalParameter.short("p9", Some(2.toShort)) shouldBe FormalParameter("p9", CoreTypes.ShortT, Some(2.toShort))
    FormalParameter.string("p10", Some("hello")) shouldBe FormalParameter("p10", CoreTypes.StringT, Some("hello"))
  }
}
