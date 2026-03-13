package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.ast.AstBuilders._
import software.kes.scaletta.scanner.Token
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class ParserTest extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
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
            call(ref("f")).group().build()
          }
        }

        it("should report an error for trailing dot (a.)") {
          "a." shouldFailWith (ParseError.UnexpectedToken(Token.EndOfInput) at 2) producing {
            ref("a")
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
          "1: ;" shouldFailWith (ParseError.UnexpectedToken(Token.Semicolon) at 3) producing {
            lit(1)
          }
        }

        it("should handle invalid type identifier (1: 2 + 3)") {
          "1: 2 + 3" shouldFailWith (ParseError.UnexpectedToken(Token.IntLiteral(2)) at 3) producing {
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
        "{ def f: = 1; f }" shouldRecoverWith (ParseError.UnexpectedToken(Token.Eq) at 9) producing {
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
        ("{ def f(x: ): Int = 1; f }" shouldRecoverWith containErrorOfType[ParseError.UnexpectedToken])
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
          ("{ def f(x: Int = ): Int = 1; f }" shouldRecoverWith containErrorOfType[ParseError.MissingExpression])
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
          |}""".stripMargin shouldFailWith containErrorOfType[ParseError.UnexpectedToken] producing {
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
        "{ val x = 1 val y = 2; x + y }" shouldFailWith containErrorOfType[ParseError.UnexpectedToken]
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
        callSimple(ref("f"), lit(1), lit(2))
      }
    }

    it("should recover from an unexpected token in a function call") {
      "f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5) producing {
        callSimple(ref("f"), lit(1), lit(2))
      }
    }

    it("should collect multiple errors in a function call") {
      "f(1, @, #, 2)" shouldRecoverWith(
        ParseError.UnexpectedToken(Token.At) at 5,
        ParseError.UnexpectedToken(Token.Hash) at 8
      ) producing {
        callSimple(ref("f"), lit(1), lit(2))
      }
    }

    it("should handle an unexpected token at the start of an argument") {
      "f(@, 1)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 2) producing {
        callSimple(ref("f"), lit(1))
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
          .producing(callSimple(ref("f"), lit(1), lit(2)))
          .andNoFatalErrors()
      }

      it("should support withPartialAst for custom assertions") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .withPartialAst { ast =>
            ast shouldBe callSimple(ref("f"), lit(1), lit(2))
          }
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
      "if x > 0 a else b" shouldFailWith containErrorOfType[ParseError.UnexpectedToken]
    }

    it("should fail if else is missing") {
      "if (c) a" shouldFailWith containErrorOfType[ParseError.UnexpectedToken]
    }
  }

}
