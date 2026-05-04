package software.kes.scaletta.parser

import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.ast.AstBuilders._
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class ParserComplexExampleSpec extends AnyFunSuite with Matchers {
  private implicit val support: ParserTestSupport = new ParserTestSupport()
  private implicit val matchers: Matchers = this

  import ParserTestOps._

  // Nested Blocks and Value Bindings
  test(example) {
    """{
      |  val x = {
      |    val a = 1
      |    val b = 2
      |    a + b
      |  }
      |  x
      |}
    """.stripMargin shouldParseTo block(ref("x"),
      valId("x", block(
        infix(ref("a"), "+", ref("b")),
        valId("a", lit(1)),
        valId("b", lit(2))
      ))
    )
  }

  // Curried Function Definitions with Type Ascriptions
  test(example) {
    """{
      |  def multiply(x: Int)(y: Int): Int = x * y
      |  multiply(41)(43)
      |}""".stripMargin shouldParseTo
      block(
        call(ref("multiply")).group(lit(41)).group(lit(43)).build(),
        defDecl("multiply")
          .group(param("x", "Int"))
          .group(param("y", "Int"))
          .returnType("Int")
          .body(infix(ref("x"), "*", ref("y")))
      )
  }

  // Complex Function Calls with Named and Positional Arguments
  test(example) {
    """process(1, "data", mode = "fast", verbose = true)""" shouldParseTo
      callSimple(ref("process"),
        arg(lit(1)),
        arg(lit("data")),
        namedArg("mode", lit("fast")),
        namedArg("verbose", lit(true))
      )
  }

  // Higher-Order Function Type Ascriptions
  test(example) {
    """{
      |  val mapper: (Int, String) => Boolean = f
      |  mapper
      |}""".stripMargin shouldParseTo
      block(
        ref("mapper"),
        valDecl(pTypedId("mapper", tFunc(Vector(tName("Int"), tName("String")), tName("Boolean"))), ref("f"))
      )
  }

  // Deeply Nested Member Selection and Calls
  test(example) {
    """config.database.connection.query("SELECT *").execute()""" shouldParseTo
      callSimple(
        select(
          callSimple(
            select(select(select(ref("config"), "database"), "connection"), "query"),
            lit("SELECT *")
          ),
          "execute"
        )
      )
  }

  // Arithmetic Precedence with Grouping
  test(example) {
    """(a + b) * (c - d) / (e + 41)""" shouldParseTo
      infix(
        infix(
          infix(ref("a"), "+", ref("b")),
          "*",
          infix(ref("c"), "-", ref("d"))
        ),
        "/",
        infix(ref("e"), "+", lit(41))
      )
  }

  // Typed Patterns in Val Declarations
  test(example) {
    """{
      |  val x: Int = 41
      |  val y: String = "hello"
      |  x
      |}""".stripMargin shouldParseTo
      block(
        ref("x"),
        valTypedId("x", "Int", lit(41)),
        valTypedId("y", "String", lit("hello"))
      )
  }

  // Conditional Expressions within Blocks
  test(example) {
    """{
      |  val result = if (status == 1) "ok" else "error"
      |  result
      |}""".stripMargin shouldParseTo
      block(
        ref("result"),
        valId("result", cond(infix(ref("status"), "==", lit(1)), lit("ok"), lit("error")))
      )
  }

  // Nested Conditional Expressions
  test(example) {
    """if (a > 0) {
      |  if (b > 0) "both positive" else "a positive, b non-positive"
      |} else {
      |  if (b > 0) "a non-positive, b positive" else "both non-positive"
      |}""".stripMargin shouldParseTo
      cond(
        infix(ref("a"), ">", lit(0)),
        block(cond(infix(ref("b"), ">", lit(0)), lit("both positive"), lit("a positive, b non-positive"))),
        block(cond(infix(ref("b"), ">", lit(0)), lit("a non-positive, b positive"), lit("both non-positive")))
      )
  }

  // Complex Block with Mixed Declarations
  test(example) {
    """{
      |  lazy val cache = createCache()
      |  def lookup(key: String): Option = cache.get(key)
      |  val key = "user_1"
      |  lookup(key)
      |}""".stripMargin shouldParseTo
      block(
        callSimple(ref("lookup"), ref("key")),
        lazyValDecl(pId("cache"), callSimple(ref("createCache"))),
        defDecl("lookup").group(param("key", "String")).returnType("Option").body(callSimple(select(ref("cache"), "get"), ref("key"))),
        valId("key", lit("user_1"))
      )
  }

  // Type Applied (Generics)
  test(example) {
    """{
      |  val list: List[Int] = Nil
      |  list
      |}""".stripMargin shouldParseTo
      block(ref("list"), valDecl(pTypedId("list", tApplied("List", "Int")), ref("Nil")))
  }

  // Nested Function Calls and Blocks
  test(example) {
    """{
      |  def outer(x: Int): Int = {
      |    def inner(y: Int): Int = x + y
      |    inner(41)
      |  }
      |  outer(43)
      |}""".stripMargin shouldParseTo
      block(
        callSimple(ref("outer"), lit(43)),
        defDecl("outer").group(param("x", "Int")).returnType("Int").body(
          block(
            callSimple(ref("inner"), lit(41)),
            defDecl("inner").group(param("y", "Int")).returnType("Int").body(
              infix(ref("x"), "+", ref("y"))
            )
          )
        )
      )
  }

  private def example(implicit pos: Position): String =
    s"Example on line ${pos.lineNumber}"

}
