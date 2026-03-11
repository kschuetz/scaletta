package software.kes.scaletta.scanner

import software.kes.scaletta.common.Interpolator

sealed trait Token {
  def canBeginStatement: Boolean = true

  def canTerminateStatement: Boolean = false
}

object Token {
  case object BeginOfInput extends Token

  case object EndOfInput extends Token

  case class Error(error: ScanError) extends Token {
    override def canTerminateStatement: Boolean = true
  }

  sealed trait ReservedWord extends Token {
    def name: String

    def aliases: List[String] = Nil

    def allForms: List[String] = name :: aliases

    /**
     * A context-sensitive reserved word is one that is only reserved in certain contexts.
     * It can be considered an identifier in other contexts.
     */
    def contextSensitive: Boolean = false
  }

  sealed trait ScalaReservedWord extends ReservedWord

  case object Case extends ReservedWord {
    def name: String = "case"

    override def canBeginStatement: Boolean = false
  }

  case object Def extends ReservedWord {
    def name: String = "def"
  }

  case object Else extends ReservedWord {
    def name: String = "else"

    override def canBeginStatement: Boolean = false
  }

  case object If extends ReservedWord {
    def name: String = "if"
  }

  case object Match extends ReservedWord {
    def name: String = "match"

    override def canBeginStatement: Boolean = false
  }

  case object Val extends ReservedWord {
    def name: String = "val"
  }

  // Scala 3 keywords
  case object Then extends ReservedWord {
    def name: String = "then"

    override def canBeginStatement: Boolean = false

    override def contextSensitive: Boolean = true
  }

  case object Yield extends ReservedWord {
    def name: String = "yield"

    override def canBeginStatement: Boolean = false
  }

  sealed trait Literal extends Token {
    override def canTerminateStatement: Boolean = true
  }

  case class IntLiteral(value: Int) extends Literal

  case class LongLiteral(value: Long) extends Literal

  case class FloatLiteral(value: Float) extends Literal

  case class DoubleLiteral(value: Double) extends Literal

  case object True extends Literal with ReservedWord {
    def name: String = "true"
  }

  case object False extends Literal with ReservedWord {
    def name: String = "false"
  }

  case object Null extends Literal with ReservedWord {
    def name: String = "null"
  }

  case class CharLiteral(value: Char) extends Literal

  case class StringLiteral(value: String) extends Literal

  case class MultiLineString(value: String) extends Literal

  case class BeginInterpolatedString(interpolator: Interpolator) extends Token

  case class BeginMultiLineInterpolatedString(interpolator: Interpolator) extends Token

  case class InterpolatedPart(value: String) extends Token

  case object BeginInterpolatedEscape extends Token

  case object EndInterpolatedEscape extends Token

  case object EndInterpolatedString extends Token {
    override def canTerminateStatement: Boolean = true
  }

  sealed trait Identifier extends Token {
    def name: String

    override def canTerminateStatement: Boolean = true
  }

  object Identifier {
    case class Upper(name: String) extends Token with Identifier

    case class Lower(name: String) extends Token with Identifier

    case class Quoted(name: String) extends Token with Identifier

    case class Operator(name: String) extends Token with Identifier {
      override def canBeginStatement: Boolean = false

      override def canTerminateStatement: Boolean = false
    }
  }

  case object LParen extends Token

  case object RParen extends Token {
    override def canBeginStatement: Boolean = false

    override def canTerminateStatement: Boolean = true
  }

  case object LBracket extends Token {
    override def canBeginStatement: Boolean = false
  }

  case object RBracket extends Token {
    override def canBeginStatement: Boolean = false

    override def canTerminateStatement: Boolean = true
  }

  case object LBrace extends Token

  case object RBrace extends Token {
    override def canBeginStatement: Boolean = false

    override def canTerminateStatement: Boolean = true
  }

  case object Backtick extends Token

  case object SingleQuote extends Token

  case object DoubleQuote extends Token

  case object Dot extends Token {
    override def canBeginStatement: Boolean = false
  }

  case object Semicolon extends Token {
    override def canBeginStatement: Boolean = false

    override def canTerminateStatement: Boolean = true
  }

  case object Comma extends Token {
    override def canBeginStatement: Boolean = false
  }

  case object Newline extends Token

  case object Pipe extends ReservedWord {
    def name: String = "|"
  }

  case object Ampersand extends ReservedWord {
    def name: String = "&"
  }

  case object Underscore extends ReservedWord {
    def name: String = "_"

    override def canTerminateStatement: Boolean = true
  }

  case object Colon extends ReservedWord {
    def name: String = ":"

    override def canBeginStatement: Boolean = false
  }

  case object Eq extends ReservedWord {
    def name: String = "="

    override def canBeginStatement: Boolean = false
  }

  case object RDoubleArrow extends ReservedWord {
    def name: String = "=>"

    override def aliases: List[String] = List("⇒")

    override def canBeginStatement: Boolean = false
  }

  case object LArrow extends ReservedWord {
    def name: String = "<-"

    override def aliases: List[String] = List("←")

    override def canBeginStatement: Boolean = false
  }

  case object At extends ReservedWord {
    def name: String = "@"
  }

  case object Abstract extends ScalaReservedWord {
    def name: String = "abstract"
  }

  case object Catch extends ScalaReservedWord {
    def name: String = "catch"

    override def canBeginStatement: Boolean = false
  }

  case object Class extends ScalaReservedWord {
    def name: String = "class"
  }

  case object Do extends ScalaReservedWord {
    def name: String = "do"
  }

  case object Enum extends ScalaReservedWord {
    def name: String = "enum"
  }

  case object Export extends ScalaReservedWord {
    def name: String = "export"
  }

  case object Extends extends ScalaReservedWord {
    def name: String = "extends"

    override def canBeginStatement: Boolean = false
  }

  case object Final extends ScalaReservedWord {
    def name: String = "final"
  }

  case object Finally extends ScalaReservedWord {
    def name: String = "finally"

    override def canBeginStatement: Boolean = false
  }

  case object For extends ScalaReservedWord {
    def name: String = "for"
  }

  case object Given extends ScalaReservedWord {
    def name: String = "given"
  }

  case object Hash extends ScalaReservedWord {
    def name: String = "#"

    override def canBeginStatement: Boolean = false
  }

  case object Implicit extends ScalaReservedWord {
    def name: String = "implicit"
  }

  case object Import extends ScalaReservedWord {
    def name: String = "import"
  }

  case object Lazy extends ScalaReservedWord {
    def name: String = "lazy"
  }

  case object LowerBound extends ScalaReservedWord {
    def name: String = ">:"

    override def canBeginStatement: Boolean = false
  }

  case object New extends ScalaReservedWord {
    def name: String = "new"
  }

  case object Object extends ScalaReservedWord {
    def name: String = "object"
  }

  case object Override extends ScalaReservedWord {
    def name: String = "override"
  }

  case object Package extends ScalaReservedWord {
    def name: String = "package"
  }

  case object Private extends ScalaReservedWord {
    def name: String = "private"
  }

  case object Protected extends ScalaReservedWord {
    def name: String = "protected"
  }

  case object Return extends ScalaReservedWord {
    def name: String = "return"
  }

  case object Sealed extends ScalaReservedWord {
    def name: String = "sealed"
  }

  case object Super extends ScalaReservedWord {
    def name: String = "super"
  }

  case object This extends ScalaReservedWord {
    def name: String = "this"
  }

  case object Throw extends ScalaReservedWord {
    def name: String = "throw"
  }

  case object Trait extends ScalaReservedWord {
    def name: String = "trait"
  }

  case object Try extends ScalaReservedWord {
    def name: String = "try"
  }

  case object Type extends ScalaReservedWord {
    def name: String = "type"
  }

  case object UpperBound extends ScalaReservedWord {
    def name: String = "<:"

    override def canBeginStatement: Boolean = false
  }

  case object Using extends ScalaReservedWord {
    def name: String = "using"

    override def canBeginStatement: Boolean = false
  }

  case object Var extends ScalaReservedWord {
    def name: String = "var"
  }

  case object ViewBound extends ScalaReservedWord {
    def name: String = "<%"

    override def canBeginStatement: Boolean = false
  }

  case object While extends ScalaReservedWord {
    def name: String = "while"
  }

  case object With extends ScalaReservedWord {
    def name: String = "with"

    override def canBeginStatement: Boolean = false
  }

  val allReservedWords: Vector[ReservedWord] =
    Vector(
      Abstract, Ampersand, At, Case, Catch, Class, Colon, Def, Do, Else, Enum, Eq, Export, Extends,
      False, Final, Finally, For, Given, Hash, If, Implicit, Import, LArrow, Lazy,
      LowerBound, Match, New, Null, Object, Override, Package, Pipe, Private, Protected,
      RDoubleArrow, Return, Sealed, Super, Then, This, Throw, Trait, True, Try, Type,
      UpperBound, Using, Underscore, Val, Var, ViewBound, While, With, Yield
    )

  val reservedWordByName: Map[String, ReservedWord] =
    allReservedWords.foldLeft(Map.empty[String, ReservedWord]) {
      case (acc, token) =>
        token.aliases.foldLeft(acc.updated(token.name, token)) {
          case (acc1, alias) => acc1.updated(alias, token)
        }
    }

  val maxReservedWordLength: Int =
    reservedWordByName.values.foldLeft(0) {
      case (acc, word) => word.allForms.foldLeft(acc) {
        case (acc1, name) => acc1.max(name.length)
      }
    }
}
