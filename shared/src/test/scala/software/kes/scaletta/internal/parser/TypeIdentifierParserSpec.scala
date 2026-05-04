package software.kes.scaletta.internal.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.ast.{Identifier, TypeIdentifier}
import software.kes.scaletta.internal.scanner.Token
import software.kes.scaletta.internal.types.ConjunctionType
import software.kes.scaletta.testsupport.{ParseErrorMatchers, ParserTestSupport}
import software.kes.scaletta.util.functional.Id.Id

class TypeIdentifierParserSpec extends AnyFunSpec with Matchers {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParseErrorMatchers._
  import software.kes.scaletta.testsupport.ParserTestOps._

  describe("TypeIdentifierParser") {
    it("should parse simple names") {
      "Int".shouldParseTypeTo(tName("Int"))
      "String".shouldParseTypeTo(tName("String"))
      "MyType".shouldParseTypeTo(tName("MyType"))
    }

    it("should parse applied types") {
      "List[Int]".shouldParseTypeTo(tApplied(tName("List"), tName("Int")))
      "Map[String, Int]".shouldParseTypeTo(tApplied(tName("Map"), tName("String"), tName("Int")))
      "Option[Option[Int]]".shouldParseTypeTo(tApplied(tName("Option"), tApplied(tName("Option"), tName("Int"))))
    }

    it("should parse qualified types") {
      "foo.Bar".shouldParseTypeTo(tSelect(tName("foo"), "Bar"))
      "foo.bar.Baz".shouldParseTypeTo(tSelect(tSelect(tName("foo"), "bar"), "Baz"))
    }

    it("should parse qualified applied types") {
      "pkg.Type[Int]".shouldParseTypeTo(tApplied(tSelect(tName("pkg"), "Type"), tName("Int")))
      "pkg.sub.Type[A, B]".shouldParseTypeTo(tApplied(tSelect(tSelect(tName("pkg"), "sub"), "Type"), tName("A"), tName("B")))
    }

    it("should parse qualified type parameters") {
      "List[foo.Bar]".shouldParseTypeTo(tApplied(tName("List"), tSelect(tName("foo"), "Bar")))
      "List[(foo.Bar, Baz)]".shouldParseTypeTo(
        tApplied(tName("List"), tTuple(tSelect(tName("foo"), "Bar"), tName("Baz")))
      )
      "Map[pkg.Key, pkg.sub.Value]".shouldParseTypeTo(
        tApplied(tName("Map"), tSelect(tName("pkg"), "Key"), tSelect(tSelect(tName("pkg"), "sub"), "Value"))
      )
    }

    it("should parse union types") {
      "Int | String".shouldParseTypeTo(tUnion(tName("Int"), tName("String")))
      "Int | String | Boolean".shouldParseTypeTo(tUnion(tName("Int"), tName("String"), tName("Boolean")))
    }

    it("should parse intersection types") {
      "A & B".shouldParseTypeTo(tIntersection(tName("A"), tName("B")))
      "A & B & C".shouldParseTypeTo(tIntersection(tName("A"), tName("B"), tName("C")))
    }

    it("should respect precedence between & and |") {
      // & binds tighter than |
      "A | B & C".shouldParseTypeTo(tUnion(tName("A"), tIntersection(tName("B"), tName("C"))))
      "A & B | C".shouldParseTypeTo(tUnion(tIntersection(tName("A"), tName("B")), tName("C")))
    }

    it("should parse function types") {
      "Int => String".shouldParseTypeTo(tFunction(Vector(tName("Int")), tName("String")))
      "(Int, String) => Boolean".shouldParseTypeTo(tFunction(Vector(tName("Int"), tName("String")), tName("Boolean")))
      "() => Unit".shouldParseTypeTo(tFunction(Vector.empty, tName("Unit")))
    }

    it("should parse right-associative function types") {
      "A => B => C".shouldParseTypeTo(tFunction(Vector(tName("A")), tFunction(Vector(tName("B")), tName("C"))))
    }

    it("should parse parenthesized types") {
      "(Int)".shouldParseTypeTo(tName("Int"))
      "(Int | String)".shouldParseTypeTo(tUnion(tName("Int"), tName("String")))
      "((A))".shouldParseTypeTo(tName("A"))
    }

    it("should parse tuple types") {
      "(Int, String)".shouldParseTypeTo(tTuple(tName("Int"), tName("String")))
      "(Int, String, Boolean)".shouldParseTypeTo(tTuple(tName("Int"), tName("String"), tName("Boolean")))
      "()".shouldParseTypeTo(tTuple())
      "((Int, String), Boolean)".shouldParseTypeTo(tTuple(tTuple(tName("Int"), tName("String")), tName("Boolean")))
    }

    it("should parse complex nested types") {
      "(A | B) & C => List[D]".shouldParseTypeTo(tFunction(
        Vector(tIntersection(tUnion(tName("A"), tName("B")), tName("C"))),
        tApplied(tName("List"), tName("D"))
      ))
    }

    it("should parse function types with complex parameters") {
      "(A | B, C & D) => E".shouldParseTypeTo(tFunction(
        Vector(tUnion(tName("A"), tName("B")), tIntersection(tName("C"), tName("D"))),
        tName("E")
      ))
    }

    it("should fail on unclosed brackets") {
      "List[Int".shouldFailToParseTypeWith(ParseError.UnclosedDelimiter(Token.LBracket, Token.RBracket).at(4))
    }
  }

  private def id(name: String): Id[Identifier[Id]] = Identifier[Id](name)

  private def tName(name: String): TypeIdentifier[Id] = TypeIdentifier.Name[Id](id(name))

  private def tSelect(qualifier: TypeIdentifier[Id], name: String): TypeIdentifier[Id] =
    TypeIdentifier.Select[Id](qualifier, id(name))

  private def tApplied(qualifier: TypeIdentifier[Id], args: TypeIdentifier[Id]*): TypeIdentifier[Id] =
    TypeIdentifier.Applied[Id](qualifier, ::(args.head, args.tail.toList))

  private def tUnion(components: TypeIdentifier[Id]*): TypeIdentifier[Id] =
    TypeIdentifier.Conjunction[Id](ConjunctionType.Union, components.toVector)

  private def tIntersection(components: TypeIdentifier[Id]*): TypeIdentifier[Id] =
    TypeIdentifier.Conjunction[Id](ConjunctionType.Intersection, components.toVector)

  private def tFunction(params: Vector[TypeIdentifier[Id]], result: TypeIdentifier[Id]): TypeIdentifier[Id] =
    TypeIdentifier.Function[Id](params, result)

  private def tTuple(elements: TypeIdentifier[Id]*): TypeIdentifier[Id] =
    TypeIdentifier.Tuple[Id](elements.toVector)

}
