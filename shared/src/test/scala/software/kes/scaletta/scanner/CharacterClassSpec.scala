package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CharacterClassSpec extends AnyFunSpec with Matchers {

  import CharacterClass._

  describe("isWhitespace") {
    it("recognizes ASCII whitespace") {
      isWhitespace(' ') shouldBe true
      isWhitespace('\t') shouldBe true
      isWhitespace('\r') shouldBe true
      isWhitespace('\n') shouldBe true
    }

    it("does not recognize non-whitespace ASCII") {
      isWhitespace('a') shouldBe false
      isWhitespace('0') shouldBe false
      isWhitespace('$') shouldBe false
    }
  }

  describe("isDigit") {
    it("recognizes ASCII digits") {
      ('0' to '9').foreach(ch => isDigit(ch) shouldBe true)
    }

    it("does not recognize non-digits") {
      isDigit('a') shouldBe false
      isDigit(' ') shouldBe false
      isDigit('\u0660') shouldBe false // ARABIC-INDIC DIGIT ZERO (not an ASCII digit)
    }
  }

  describe("isLetter") {
    it("recognizes ASCII letters") {
      ('a' to 'z').foreach(ch => isLetter(ch) shouldBe true)
      ('A' to 'Z').foreach(ch => isLetter(ch) shouldBe true)
    }

    it("recognizes Unicode letters") {
      isLetter('λ') shouldBe true // GREEK SMALL LETTER LAMBDA (Lowercase)
      isLetter('Ω') shouldBe true // GREEK CAPITAL LETTER OMEGA (Uppercase)
      isLetter('ǈ') shouldBe true // LATIN CAPITAL LETTER L WITH SMALL LETTER J (Titlecase)
      isLetter('Ⅸ') shouldBe true // ROMAN NUMERAL NINE (Letter Number)
      isLetter('\u02B0') shouldBe true // MODIFIER LETTER SMALL H (Modifier Letter)
    }

    it("does not recognize non-letters") {
      isLetter('0') shouldBe false
      isLetter('_') shouldBe false
      isLetter('$') shouldBe false
      isLetter(' ') shouldBe false
    }
  }

  describe("isIdentifierStart") {
    it("recognizes valid starts") {
      isIdentifierStart('a') shouldBe true
      isIdentifierStart('A') shouldBe true
      isIdentifierStart('_') shouldBe true
      isIdentifierStart('$') shouldBe true
      isIdentifierStart('λ') shouldBe true
    }

    it("rejects invalid starts") {
      isIdentifierStart('0') shouldBe false
      isIdentifierStart(' ') shouldBe false
      isIdentifierStart('+') shouldBe false
    }
  }

  describe("isIdentifierInner") {
    it("recognizes valid inner characters") {
      isIdentifierInner('a') shouldBe true
      isIdentifierInner('0') shouldBe true
      isIdentifierInner('_') shouldBe true
      isIdentifierInner('$') shouldBe true
      isIdentifierInner('\u0300') shouldBe true // COMBINING GRAVE ACCENT (Combining Mark)
    }

    it("rejects invalid inner characters") {
      isIdentifierInner(' ') shouldBe false
      isIdentifierInner('+') shouldBe false
    }
  }

  describe("isOperator") {
    it("recognizes ASCII operators") {
      isOperator('+') shouldBe true
      isOperator('-') shouldBe true
      isOperator('*') shouldBe true
      isOperator('/') shouldBe true
      isOperator('=') shouldBe true
      isOperator('<') shouldBe true
      isOperator('>') shouldBe true
      isOperator('&') shouldBe true
      isOperator('|') shouldBe true
      isOperator('^') shouldBe true
      isOperator('~') shouldBe true
      isOperator('!') shouldBe true
      isOperator('?') shouldBe true
      isOperator('%') shouldBe true
      isOperator(':') shouldBe true
      isOperator('#') shouldBe true
      isOperator('@') shouldBe true
      isOperator('\\') shouldBe true
    }

    it("recognizes Unicode symbols") {
      isOperator('∑') shouldBe true // N-ARY SUMMATION (Math Symbol)
      isOperator('∞') shouldBe true // INFINITY (Math Symbol)
      isOperator('©') shouldBe true // COPYRIGHT SIGN (Other Symbol)
    }

    it("rejects non-operators") {
      isOperator('a') shouldBe false
      isOperator('0') shouldBe false
      isOperator('_') shouldBe false
      isOperator('$') shouldBe false
      isOperator(' ') shouldBe false
      isOperator('(') shouldBe false
      isOperator(')') shouldBe false
      isOperator('[') shouldBe false
      isOperator(']') shouldBe false
      isOperator('{') shouldBe false
      isOperator('}') shouldBe false
      isOperator(',') shouldBe false
      isOperator('.') shouldBe false
      isOperator(';') shouldBe false
      isOperator('`') shouldBe false
      isOperator('\'') shouldBe false
      isOperator('"') shouldBe false
    }
  }

  describe("isUppercase") {
    it("recognizes uppercase letters") {
      isUppercase('A') shouldBe true
      isUppercase('Ω') shouldBe true
    }

    it("rejects non-uppercase letters") {
      isUppercase('a') shouldBe false
      isUppercase('λ') shouldBe false
      isUppercase('0') shouldBe false
      isUppercase('_') shouldBe false
    }
  }
}
