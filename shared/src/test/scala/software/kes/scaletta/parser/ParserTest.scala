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
  import software.kes.scaletta.testsupport.ParseErrorMatchers.{atIndex => errorAtIndex, _}
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
          "f(x, y)" shouldParseTo call(ref("f"), ref("x"), ref("y"))
        }

        it("should parse a function call with no arguments (f())") {
          "f()" shouldParseTo call(ref("f"))
        }

        it("should parse a nested function call (f(g(x)))") {
          "f(g(x))" shouldParseTo call(ref("f"), call(ref("g"), ref("x")))
        }

        it("should parse a call on an expression target ((f + g)(x))") {
          "(f + g)(x)" shouldParseTo call(infix(ref("f"), "+", ref("g")), ref("x"))
        }

        it("should parse multiple argument groups (f(x)(y))") {
          "f(x)(y)" shouldParseTo multiCall(ref("f"), Vector(Vector(ref("x")), Vector(ref("y"))))
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
          "a.b(c).d" shouldParseTo select(call(select(ref("a"), "b"), ref("c")), "d")
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
        "{ def f: Int = 1; f }" shouldParseTo block(ref("f"), defDecl("f", Vector.empty, "Int", lit(1)))
      }

      it("should parse a def declaration with a function return type") {
        "{ def g: Int => String = s; g }" shouldParseTo {
          block(ref("g"), defDecl("g", Vector.empty, Some(tFunc(Vector(tName("Int")), tName("String"))), ref("s")))
        }
      }

      it("should recover from a malformed return type in a def declaration") {
        "{ def f: = 1; f }" shouldRecoverWith (ParseError.UnexpectedToken(Token.Eq) at 9) producing {
          block(ref("f"), defDecl("f", Vector.empty, None, lit(1)))
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
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should handle a missing expression in an argument list") {
      "f(1, , 2)" shouldRecoverWith (ParseError.MissingExpression("argument") at 5) producing {
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should recover from an unexpected token in a function call") {
      "f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5) producing {
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should collect multiple errors in a function call") {
      "f(1, @, #, 2)" shouldRecoverWith(
        ParseError.UnexpectedToken(Token.At) at 5,
        ParseError.UnexpectedToken(Token.Hash) at 8
      ) producing {
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should handle an unexpected token at the start of an argument") {
      "f(@, 1)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 2) producing {
        call(ref("f"), lit(1))
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
          .producing(call(ref("f"), lit(1), lit(2)))
          .andNoFatalErrors()
      }

      it("should support withPartialAst for custom assertions") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .withPartialAst { ast =>
            ast shouldBe call(ref("f"), lit(1), lit(2))
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
