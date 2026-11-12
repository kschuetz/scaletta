package software.kes.scaletta.internal.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.internal.ast.AstBuilders._
import software.kes.scaletta.internal.ast.ParsingPhase._
import software.kes.scaletta.internal.scanner.Token
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class ParserSpec extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParserTestOps._
  import software.kes.scaletta.testsupport.ParseErrorMatchers._

  private val errorAtIndex = atIndex _
  import software.kes.scaletta.testsupport.ParseWarningMatchers._

  describe("Parser") {
    describe("Simple Expressions") {
      it("should parse various literals and identifiers correctly") {
        val simpleExpressions = Table(
          ("input", "expectedAst"),
          ("123", lit(123)),
          ("\"hello\"", lit("hello")),
          ("true", lit(true)),
          ("false", lit(false)),
          ("null", litNull),
          ("()", litUnit),
          ("foo", ref("foo"))
        )

        forAll(simpleExpressions) { (input, expected) =>
          input shouldParseTo expected
        }
      }
    }

    describe("Expressions") {
      describe("Arithmetic & Precedence") {
        it("should respect operator precedence and grouping") {
          val arithmeticTests = Table(
            ("input", "expectedAst"),
            ("1 + 2", infix(lit(1), "+", lit(2))),
            ("1 + 2 * 3", infix(lit(1), "+", infix(lit(2), "*", lit(3)))),
            ("1 * 2 + 3", infix(infix(lit(1), "*", lit(2)), "+", lit(3))),
            ("(1 + 2) * 3", infix(infix(lit(1), "+", lit(2)), "*", lit(3)))
          )

          forAll(arithmeticTests) { (input, expected) =>
            input shouldParseTo expected
          }
        }
      }

      describe("Function Calls") {
        it("should parse a standard function call (f(x, y))") {
          "f(x, y)" shouldParseTo callSimple(ref("f"), ref("x"), ref("y"))
        }

        it("should parse a function call with no arguments (f())") {
          "f()" shouldParseTo call(ref("f")).build()
        }

        it("should parse a nested function call (f(g(x)))") {
          "f(g(x))" shouldParseTo callSimple(ref("f"), callSimple(ref("g"), ref("x")))
        }

        it("should parse a call on an expression target ((f + g)(x))") {
          "(f + g)(x)" shouldParseTo callSimple(infix(ref("f"), "+", ref("g")), ref("x"))
        }

        it("should parse multiple argument groups (f(x)(y))") {
          "f(x)(y)" shouldParseTo call(ref("f")).group(ref("x")).group(ref("y")).build()
        }

        it("should parse an alphanumeric infix operator (a plus b)") {
          "a plus b" shouldParseWithWarnings (ParseWarning.SuspiciousInfixExpression("plus") at 2) producing {
            infix(ref("a"), "plus", ref("b"))
          }
        }

        it("should parse member selection (a.b)") {
          "a.b" shouldParseTo select(ref("a"), "b")
        }

        it("should parse nested member selection (a.b.c)") {
          "a.b.c" shouldParseTo select(select(ref("a"), "b"), "c")
        }

        it("should parse mixed selection and calls (a.b(c).d)") {
          "a.b(c).d" shouldParseTo select(callSimple(select(ref("a"), "b"), ref("c")), "d")
        }

        it("should parse a function call with named arguments") {
          "f(x = 1, y = \"foo\")" shouldParseTo callSimple(ref("f"), namedArg("x", lit(1)), namedArg("y", lit("foo")))
        }

        it("should parse a function call with mixed positional and named arguments") {
          "f(1, y = 2)" shouldParseTo callSimple(ref("f"), arg(lit(1)), namedArg("y", lit(2)))
        }

        it("should fail when a positional argument follows a named argument") {
          "f(x = 1, 2)" shouldFailWith (ParseError.PositionalAfterNamedArgument at 9) producing {
            callSimple(ref("f"), namedArg("x", lit(1)), arg(lit(2)))
          }
        }

        it("should fail when a named argument is missing its value") {
          "f(x = )" shouldFailWith(ParseError.UnexpectedToken(Token.RParen) at 6, ParseError.UnclosedDelimiter(Token.LParen, Token.RParen) at 1) producing {
            callSimple(ref("f"), namedArg("x", errUnexpected(Token.RParen)))
          }
        }

        it("should report an error for trailing dot (a.)") {
          "a." shouldFailWith (ParseError.ExpectedIdentifier(Token.EndOfInput, "selection") spanning(2, 2)) producing {
            select(ref("a"), "<error>")
          }
        }
      }

      describe("Type Ascriptions") {
        it("should parse a simple type ascription (1: Int)") {
          "1: Int" shouldParseTo typed(lit(1), "Int")
        }

        it("should parse complex types (x: (Int, String) => Boolean)") {
          val complexType = tFunc(Vector(tName("Int"), tName("String")), tName("Boolean"))
          "x: (Int, String) => Boolean" shouldParseTo typed(ref("x"), complexType)
        }

        it("should respect precedence (1 + 2 : Int)") {
          "1 + 2 : Int" shouldParseTo typed(infix(lit(1), "+", lit(2)), "Int")
        }

        it("should handle missing type (1: ;)") {
          "1: ;" shouldFailWith (ParseError.ExpectedIdentifier(Token.Semicolon, "type") spanning(3, 3)) producing {
            lit(1)
          }
        }

        it("should handle invalid type identifier (1: 2 + 3)") {
          "1: 2 + 3" shouldFailWith (ParseError.ExpectedIdentifier(Token.IntLiteral(2), "type") spanning(3, 3)) producing {
            lit(1)
          }
        }
      }
    }

    describe("Scanner Exhaustion") {
      it("should fail if there is trailing garbage") {
        "123 garbage" shouldFailWith (ParseError.ExtraToken(Token.Identifier.Lower("garbage"), "end of input") at 4) producing {
          lit(123)
        }
      }

      it("should allow trailing garbage when using shouldParsePartiallyTo") {
        "123 garbage" shouldParsePartiallyTo lit(123)
      }
    }

    describe("Declarations") {
      it("should parse a simple val declaration") {
        "{ val x = 1; x }" shouldParseTo block(ref("x"), valId("x", lit(1)))
      }

      it("should parse a typed val declaration") {
        "{ val x: Int = 1; x }" shouldParseTo block(ref("x"), valTypedId("x", "Int", lit(1)))
      }

      it("should parse a val declaration with a unit pattern") {
        "{ val () = (); () }" shouldParseTo block(litUnit, valDecl(pLit(Literal.unit()), litUnit))
      }

      describe("Patterns in Declarations") {
        it("should parse literal patterns in val declarations") {
          "{ val 41 = x; 0 }" shouldParseTo block(lit(0), valDecl(pLit(Literal.int(41)), ref("x")))
          "{ val \"hello\" = x; 0 }" shouldParseTo block(lit(0), valDecl(pLit(Literal.string("hello")), ref("x")))
          "{ val true = x; 0 }" shouldParseTo block(lit(0), valDecl(pLit(Literal.true_()), ref("x")))
          "{ val false = x; 0 }" shouldParseTo block(lit(0), valDecl(pLit(Literal.false_()), ref("x")))
          "{ val null = x; 0 }" shouldParseTo block(lit(0), valDecl(pLit(Literal.null_()), ref("x")))
        }

        it("should parse tuple patterns in val declarations") {
          "{ val (a, b) = pair; a }" shouldParseTo block(ref("a"), valDecl(pTuple(pId("a"), pId("b")), ref("pair")))
          "{ val (a, (b, c)) = nested; a }" shouldParseTo block(ref("a"), valDecl(pTuple(pId("a"), pTuple(pId("b"), pId("c"))), ref("nested")))
          "{ val (a) = x; a }" shouldParseTo block(ref("a"), valDecl(pId("a"), ref("x")))
        }

        it("should parse as-patterns in val declarations") {
          "{ val all @ (head, tail) = pair; all }" shouldParseTo block(ref("all"), valDecl(pAs("all", pTuple(pId("head"), pId("tail"))), ref("pair")))
        }

        it("should parse product patterns in val declarations") {
          "{ val Some(inner) = opt; inner }" shouldParseTo block(ref("inner"), valDecl(pProduct(tName("Some"), pId("inner")), ref("opt")))
          "{ val Pair(a, b) = p; a }" shouldParseTo block(ref("a"), valDecl(pProduct(tName("Pair"), pId("a"), pId("b")), ref("p")))
          "{ val None() = opt; 0 }" shouldParseTo block(lit(0), valDecl(pProduct(tName("None")), ref("opt")))
        }

        it("should parse typed patterns in val declarations") {
          "{ val (a: Int, b: String) = pair; a }" shouldParseTo block(ref("a"), valDecl(pTuple(pTypedId("a", "Int"), pTypedId("b", "String")), ref("pair")))
          "{ val opt: Option[Int] = x; opt }" shouldParseTo block(ref("opt"), valDecl(pTyped(pId("opt"), tApplied("Option", "Int")), ref("x")))
          "{ val item @ (x: Int) = data; item }" shouldParseTo block(ref("item"), valDecl(pAs("item", pTypedId("x", "Int")), ref("data")))
        }

        it("should parse patterns in lazy val declarations") {
          "{ lazy val (a, b) = pair; a }" shouldParseTo block(ref("a"), lazyValDecl(pTuple(pId("a"), pId("b")), ref("pair")))
          "{ lazy val Some(x) = opt; x }" shouldParseTo block(ref("x"), lazyValDecl(pProduct(tName("Some"), pId("x")), ref("opt")))
        }
      }

      it("should parse a lazy val declaration") {
        "{ lazy val x = 1; x }" shouldParseTo block(ref("x"), lazyValDecl(pId("x"), lit(1)))
      }

      it("should parse a simple def declaration") {
        "{ def f = 1; f }" shouldParseTo block(ref("f"), defSimple("f", lit(1)))
      }

      it("should parse a def declaration with a return type") {
        "{ def f: Int = 1; f }" shouldParseTo block(ref("f"), defDecl("f").returnType("Int").body(lit(1)))
      }

      it("should parse a def declaration with a function return type") {
        "{ def g: Int => String = s; g }" shouldParseTo {
          block(ref("g"), defDecl("g").returnType(tFunc(Vector(tName("Int")), tName("String"))).body(ref("s")))
        }
      }

      it("should recover from a malformed return type in a def declaration") {
        "{ def f: = 1; f }" shouldRecoverWith (ParseError.ExpectedIdentifier(Token.Eq, "type") spanning(9, 9)) producing {
          block(ref("f"), defDecl("f").body(lit(1)))
        }
      }

      it("should parse a def declaration with a single parameter group") {
        "{ def f(x: Int): Int = x; f(1) }" shouldParseTo {
          block(callSimple(ref("f"), lit(1)), defDecl("f").group(param("x", "Int")).returnType("Int").body(ref("x")))
        }
      }

      it("should parse a def declaration with multiple parameters") {
        "{ def add(x: Int, y: Int): Int = x + y; add(1, 2) }" shouldParseTo {
          block(callSimple(ref("add"), lit(1), lit(2)), defDecl("add").group(param("x", "Int"), param("y", "Int")).returnType("Int").body(infix(ref("x"), "+", ref("y"))))
        }
      }

      it("should parse a def declaration with multiple parameter groups (currying)") {
        "{ def g(x: Int)(y: String): Boolean = true; g(1)(\"hi\") }" shouldParseTo {
          block(call(ref("g")).group(lit(1)).group(lit("hi")).build(),
            defDecl("g").group(param("x", "Int")).group(param("y", "String")).returnType("Boolean").body(lit(true)))
        }
      }

      it("should handle error in parameter group but continue parsing") {
        ("{ def f(x: ): Int = 1; f }" shouldRecoverWith containErrorOfType[ParseError.ExpectedIdentifier])
          .producing {
            block(ref("f"), defDecl("f").group().returnType("Int").body(lit(1)))
          }.ignoringAst()
      }

      it("should report VariadicParameterMustBeLast when a variadic parameter is not last") {
        ("{ def f(x: Int*, y: Int): Int = 1; f }" shouldRecoverWith containErrorOfType[ParseError.VariadicParameterMustBeLast.type])
          .ignoringAst()
      }

      describe("Default Parameter Values") {
        it("should parse a parameter with a default value") {
          "{ def f(x: Int = 1): Int = x; f() }" shouldParseTo {
            block(callSimple(ref("f")),
              defDecl("f").group(param("x", "Int", lit(1))).returnType("Int").body(ref("x")))
          }
        }

        it("should parse multiple parameters with default values") {
          "{ def f(x: Int = 1, y: Int = 41): Int = x + y; f() }" shouldParseTo {
            block(callSimple(ref("f")),
              defDecl("f").group(param("x", "Int", lit(1)), param("y", "Int", lit(41))).returnType("Int").body(infix(ref("x"), "+", ref("y"))))
          }
        }

        it("should parse a default value in the middle of a parameter list") {
          "{ def f(x: Int = 1, y: Int): Int = x + y; f(y = 2) }" shouldParseTo {
            block(callSimple(ref("f"), namedArg("y", lit(2))),
              defDecl("f").group(param("x", "Int", lit(1)), param("y", "Int")).returnType("Int").body(infix(ref("x"), "+", ref("y"))))
          }
        }

        it("should parse default values in multiple parameter groups") {
          "{ def f(x: Int = 1)(y: Int = 43): Int = x + y; f()() }" shouldParseTo {
            block(call(ref("f")).group().group().build(),
              defDecl("f").group(param("x", "Int", lit(1))).group(param("y", "Int", lit(43))).returnType("Int").body(infix(ref("x"), "+", ref("y"))))
          }
        }

        it("should report an error for variadic parameter with a default value") {
          ("{ def f(x: Int* = 1): Int = 1; f }" shouldRecoverWith containErrorWithMessage("Variadic parameter cannot have a default value"))
            .ignoringAst()
        }

        it("should report an error for missing expression after =") {
          ("{ def f(x: Int = ): Int = 1; f }" shouldRecoverWith containErrorOfType[ParseError.UnexpectedToken])
            .ignoringAst()
        }
      }
    }

    describe("Blocks") {
      it("should parse a simple block with only a result expression") {
        "{ 123 }" shouldParseTo block(lit(123))
      }

      it("should parse a block with one val declaration") {
        """{
          |  val x = 1
          |  x
          |}""".stripMargin shouldParseTo block(ref("x"), valId("x", lit(1)))
      }

      it("should parse a block with one typed val declaration") {
        """{
          |  val x: Int = 1
          |  x
          |}""".stripMargin shouldParseTo block(ref("x"), valTypedId("x", "Int", lit(1)))
      }

      it("should parse a block with a typed wildcard val declaration") {
        """{
          |  val _: String = "hello"
          |  123
          |}""".stripMargin shouldParseTo block(lit(123), valDecl(pWildTyped("String"), lit("hello")))
      }

      it("should handle malformed type in pattern (val x: = 1)") {
        """{
          |  val x: = 1
          |  x
          |}""".stripMargin shouldFailWith containErrorOfType[ParseError.ExpectedIdentifier] producing {
          block(ref("x"), valId("x", lit(1)))
        }
      }

      it("should parse a block with multiple val declarations") {
        """{
          |  val x = 1
          |  val y = 2
          |  x + y
          |}""".stripMargin shouldParseTo block(
          infix(ref("x"), "+", ref("y")),
          valId("x", lit(1)),
          valId("y", lit(2))
        )
      }

      it("should parse a block with lazy val and def") {
        """{
          |  lazy val x = 1
          |  def f = x
          |  f
          |}""".stripMargin shouldParseTo block(
          ref("f"),
          lazyValDecl(pId("x"), lit(1)),
          defSimple("f", ref("x"))
        )
      }

      it("should require a separator between declarations") {
        "{ val x = 1 val y = 2; x + y }" shouldFailWith containErrorOfType[ParseError.ExpectedToken]
      }
    }
  }

  describe("Parser Error Recovery") {
    it("should handle an unclosed parenthesis in an expression") {
      "(1 + 2" shouldRecoverWith (ParseError.UnclosedDelimiter(Token.LParen, Token.RParen) at 0) producing {
        infix(lit(1), "+", lit(2))
      }
    }

    it("should handle an unclosed parenthesis in a function call") {
      "f(1, 2" shouldRecoverWith (ParseError.UnclosedDelimiter(Token.LParen, Token.RParen) at 1) producing {
        callSimple(ref("f"), lit(1), lit(2))
      }
    }

    it("should handle a missing expression in an argument list") {
      "f(1, , 2)" shouldRecoverWith (ParseError.MissingExpression("argument") at 5) producing {
        callSimple(ref("f"), lit(1), errMissing("argument"), lit(2))
      }
    }

    it("should recover from an unexpected token in a function call") {
      "f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5) producing {
        callSimple(ref("f"), lit(1), errUnexpected(Token.At), lit(2))
      }
    }

    it("should collect multiple errors in a function call") {
      "f(1, @, #, 2)" shouldRecoverWith(
        ParseError.UnexpectedToken(Token.At) at 5,
        ParseError.UnexpectedToken(Token.Hash) at 8
      ) producing {
        callSimple(ref("f"), lit(1), errUnexpected(Token.At), errUnexpected(Token.Hash), lit(2))
      }
    }

    it("should handle an unexpected token at the start of an argument") {
      "f(@, 1)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 2) producing {
        callSimple(ref("f"), errUnexpected(Token.At), lit(1))
      }
    }

    describe("Flexible Error Matchers") {
      it("should support containErrorOfType") {
        "f(1, @, 2)" shouldRecoverWith containErrorOfType[ParseError.UnexpectedToken]
      }

      it("should support atIndex with errorOfType") {
        "f(1, @, #, 2)" shouldRecoverWith {
          errorAtIndex(0)(errorOfType[ParseError.UnexpectedToken]) and
            errorAtIndex(1)(errorOfType[ParseError.UnexpectedToken])
        }
      }

      it("should support soft failure check (ignoringAst)") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .ignoringAst()
      }

      it("should support multiple assertions with chaining") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .producing(callSimple(ref("f"), lit(1), errUnexpected(Token.At), lit(2)))
          .andNoFatalErrors()
      }

      it("should support withPartialAst for custom assertions") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .withPartialAst { ast =>
            ast shouldBe callSimple(ref("f"), lit(1), errUnexpected(Token.At), lit(2))
          }
      }
    }
  }

  describe("Match Expressions") {
    describe("Basic Matching") {
      it("should parse match expressions with literal and wildcard patterns") {
        "x match { case 0 => 41; case _ => 43 }" shouldParseTo matchExpr(
          ref("x"),
          caseExpr(pLit(Literal.int(0)), lit(41)),
          caseExpr(pWild, lit(43))
        )
      }

      it("should parse match expressions with identifier, tuple, and product patterns") {
        "x match { case (a, b) => 41; case Some(v) => 43 }" shouldParseTo matchExpr(
          ref("x"),
          caseExpr(pTuple(pId("a"), pId("b")), lit(41)),
          caseExpr(pProduct("Some", pId("v")), lit(43))
        )
      }
    }

    describe("Guards") {
      it("should parse case clauses with guards") {
        "x match { case n if n > 0 => n; case _ => 0 }" shouldParseTo matchExpr(
          ref("x"),
          caseGuarded(pId("n"), infix(ref("n"), ">", lit(0)), ref("n")),
          caseExpr(pWild, lit(0))
        )
      }
    }

    describe("Precedence & Infix Interactions") {
      it("should parse arithmetic expressions as match target") {
        "1 + 2 match { case 3 => 41 }" shouldParseTo matchExpr(
          infix(lit(1), "+", lit(2)),
          caseExpr(pLit(Literal.int(3)), lit(41))
        )
      }

      it("should parse member selection as match target") {
        "user.age match { case 18 => true; case _ => false }" shouldParseTo matchExpr(
          select(ref("user"), "age"),
          caseExpr(pLit(Literal.int(18)), lit(true)),
          caseExpr(pWild, lit(false))
        )
      }

      it("should parse nested match expressions in case bodies") {
        "x match { case 0 => y match { case 1 => 41 } }" shouldParseTo matchExpr(
          ref("x"),
          caseExpr(pLit(Literal.int(0)), matchExpr(ref("y"), caseExpr(pLit(Literal.int(1)), lit(41))))
        )
      }
    }

    describe("Tuples") {
      it("should parse tuple expressions") {
        val tupleTests = Table(
          ("input", "expectedAst"),
          ("(1, 2)", tuple(lit(1), lit(2))),
          ("(1, \"two\", true)", tuple(lit(1), lit("two"), lit(true))),
          ("(a, b, c, d)", tuple(ref("a"), ref("b"), ref("c"), ref("d")))
        )

        forAll(tupleTests) { (input, expected) =>
          input shouldParseTo expected
        }
      }
    }

    describe("Lambda Expressions") {
      describe("Single Parameters") {
        it("should parse a bare identifier parameter (x => x + 1)") {
          "x => x + 1" shouldParseTo lambda(infix(ref("x"), "+", lit(1)), lambdaParam("x"))
        }

        it("should parse a parenthesized untyped parameter ((x) => x + 1)") {
          "(x) => x + 1" shouldParseTo lambda(infix(ref("x"), "+", lit(1)), lambdaParam("x"))
        }

        it("should parse a parenthesized typed parameter ((x: Int) => x + 1)") {
          "(x: Int) => x + 1" shouldParseTo lambda(infix(ref("x"), "+", lit(1)), lambdaParam("x", "Int"))
        }
      }

      describe("Multiple Parameters") {
        it("should parse multiple typed parameters ((x: Int, y: String) => x)") {
          "(x: Int, y: String) => x" shouldParseTo lambda(
            ref("x"),
            lambdaParam("x", "Int"),
            lambdaParam("y", "String")
          )
        }

        it("should parse multiple untyped parameters ((x, y) => x + y)") {
          "(x, y) => x + y" shouldParseTo lambda(
            infix(ref("x"), "+", ref("y")),
            lambdaParam("x"),
            lambdaParam("y")
          )
        }

        it("should parse mixed typed and untyped parameters ((x: Int, y) => x)") {
          "(x: Int, y) => x" shouldParseTo lambda(
            ref("x"),
            lambdaParam("x", "Int"),
            lambdaParam("y")
          )
        }
      }

      describe("Zero Parameters") {
        it("should parse a nullary lambda (() => 41)") {
          "() => 41" shouldParseTo lambda(lit(41))
        }
      }

      describe("Chaining & Nested Lambdas") {
        it("should parse curried/chained lambdas (x => y => x + y)") {
          "x => y => x + y" shouldParseTo lambda(
            lambda(
              infix(ref("x"), "+", ref("y")),
              lambdaParam("y")
            ),
            lambdaParam("x")
          )
        }

        it("should parse lambdas returning blocks ((x: Int) => { val y = x + 1; y })") {
          "(x: Int) => { val y = x + 1; y }" shouldParseTo lambda(
            block(ref("y"), valId("y", infix(ref("x"), "+", lit(1)))),
            lambdaParam("x", "Int")
          )
        }

        it("should parse lambdas returning match expressions ((x: Int) => x match { case 0 => 41; case _ => 43 })") {
          "(x: Int) => x match { case 0 => 41; case _ => 43 }" shouldParseTo lambda(
            matchExpr(
              ref("x"),
              caseExpr(pLit(Literal.int(0)), lit(41)),
              caseExpr(pWild, lit(43))
            ),
            lambdaParam("x", "Int")
          )
        }
      }

      describe("Calls & Declarations") {
        it("should parse a lambda as an argument in a function call") {
          "map(items, (x: Int) => x + 1)" shouldParseTo callSimple(
            ref("map"),
            ref("items"),
            lambda(infix(ref("x"), "+", lit(1)), lambdaParam("x", "Int"))
          )
        }

        it("should parse a lambda assigned in a val declaration") {
          "{ val f = (x: Int) => x * 2; f }" shouldParseTo block(
            ref("f"),
            valId("f", lambda(infix(ref("x"), "*", lit(2)), lambdaParam("x", "Int")))
          )
        }
      }

      describe("Error Recovery") {
        it("should report diagnostic and recover on invalid parameter expression ((1 + 2) => 3)") {
          "(1 + 2) => 3" shouldFailWith (ParseError.InvalidLambdaParameter at 0)
        }

        it("should report diagnostic and recover on missing lambda body (x =>)") {
          "x =>" shouldFailWith (ParseError.ExpectedIdentifier(Token.EndOfInput, "expression") at 4)
        }

        it("should report diagnostic on unparenthesized typed lambda parameter (x: Int => x + 1)") {
          "x: Int => x + 1" shouldFailWith (ParseError.LambdaParameterNeedsParentheses at 0)
        }
      }
    }

    describe("Multi-line Formatting & Delimiters") {
      it("should parse newline-separated cases without explicit semicolons") {
        """x match {
          |  case 0 => 41
          |  case _ => 43
          |}""".stripMargin shouldParseTo matchExpr(
          ref("x"),
          caseExpr(pLit(Literal.int(0)), lit(41)),
          caseExpr(pWild, lit(43))
        )
      }

      it("should parse explicit semicolon-separated cases on a single line") {
        "x match { case 0 => 41; case _ => 43; }" shouldParseTo matchExpr(
          ref("x"),
          caseExpr(pLit(Literal.int(0)), lit(41)),
          caseExpr(pWild, lit(43))
        )
      }
    }

    describe("Error Recovery") {
      it("should report error on missing closing brace") {
        "x match { case 0 => 41" shouldFailWith containErrorOfType[ParseError.UnclosedDelimiter]
      }

      it("should report error on missing => in case clause") {
        "x match { case 0 41 }" shouldFailWith containErrorOfType[ParseError.ExpectedToken]
      }

      it("should parse empty match body") {
        "x match { }" shouldParseTo matchExpr(ref("x"))
      }
    }
  }

  describe("Conditionals") {
    it("should parse a standard if-else expression with parentheses") {
      "if (x > 0) a else b" shouldParseTo cond(infix(ref("x"), ">", lit(0)), ref("a"), ref("b"))
    }

    it("should parse an if-else expression with then keyword and no parentheses") {
      "if x > 0 then a else b" shouldParseTo cond(infix(ref("x"), ">", lit(0)), ref("a"), ref("b"))
    }

    it("should parse an if-else expression with both parentheses and then keyword") {
      "if (x > 0) then a else b" shouldParseTo cond(infix(ref("x"), ">", lit(0)), ref("a"), ref("b"))
    }

    it("should parse nested if-else expressions") {
      "if (c1) if (c2) a else b else c" shouldParseTo cond(ref("c1"), cond(ref("c2"), ref("a"), ref("b")), ref("c"))
    }

    it("should parse if-else with block branches") {
      """if (c) {
        |  val x = 1
        |  x
        |} else {
        |  0
        |}""".stripMargin shouldParseTo cond(ref("c"), block(ref("x"), valId("x", lit(1))), block(lit(0)))
    }

    it("should fail if then/parens are missing") {
      "if x > 0 a else b" shouldFailWith(
        ParseError.ExpectedIdentifier(Token.Else, "expression") spanning(11, 14),
        ParseError.ExpectedToken(Token.Then, Token.Identifier.Lower("b"), "conditional") spanning(16, 16),
        ParseError.ExpectedToken(Token.Else, Token.EndOfInput, "conditional") spanning(17, 17)
      )
    }

    it("should fail if else is missing") {
      "if (a) b".shouldFailWith(containErrorOfType[ParseError.ExpectedToken])
    }
  }

  describe("diagnostics") {
    it("should report ExpectedIdentifier when an expression is expected but a structural boundary is found") {
      "if (" shouldFailWith(
        ParseError.ExpectedIdentifier(Token.EndOfInput, "expression") spanning(4, 4),
        ParseError.UnclosedDelimiter(Token.LParen, Token.RParen) spanning(3, 4),
        ParseError.ExpectedIdentifier(Token.EndOfInput, "expression") spanning(4, 4),
        ParseError.ExpectedToken(Token.Else, Token.EndOfInput, "conditional") spanning(4, 4)
      )
    }

    it("should report ExpectedToken when 'then' is missing in an if expression") {
      "if x a else b" shouldFailWith(
        ParseError.ExpectedIdentifier(Token.Else, "expression") spanning(7, 10),
        ParseError.ExpectedToken(Token.Then, Token.Identifier.Lower("b"), "conditional") spanning(12, 12),
        ParseError.ExpectedToken(Token.Else, Token.EndOfInput, "conditional") spanning(13, 13)
      )
    }

    it("should report ExpectedIdentifier in a val declaration") {
      "{ val = 1; x }" shouldFailWith(
        ParseError.ExpectedIdentifier(Token.Eq, "pattern") spanning(6, 6),
        ParseError.ExpectedToken(Token.Eq, Token.IntLiteral(1), "val declaration") spanning(8, 8)
      )
    }

    it("should report ExpectedIdentifier in a def declaration") {
      "{ def (x: Int) = 1; f }" shouldFailWith containErrorOfType[ParseError.ExpectedIdentifier]
    }

    it("should report ExpectedToken for missing '=' in a def declaration") {
      "{ def f(x: Int) 1; f }" shouldFailWith (ParseError.ExpectedToken(Token.Eq, Token.IntLiteral(1), "def declaration") spanning(16, 16))
    }

    it("should report ExpectedIdentifier in selection") {
      "a." shouldFailWith (ParseError.ExpectedIdentifier(Token.EndOfInput, "selection") spanning(2, 2))
    }

    it("should report ExpectedToken for unclosed tuple pattern in val declaration") {
      "{ val (a, b = x; 0 }" shouldFailWith containErrorOfType[ParseError.ExpectedToken]
    }
  }
}
