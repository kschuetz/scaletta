package software.kes.scaletta.scanner

sealed trait Token {
  def canBeginStatement: Boolean = true

  def canTerminateStatement: Boolean = false
}

object Token {
  case object BeginOfInput extends Token

  sealed trait ReservedWord extends Token {
    def name: String

    def aliases: List[String] = Nil

    def allForms: List[String] = name :: aliases
  }

  case object Case extends ReservedWord {
    def name: String = "case"
  }

  case object Def extends ReservedWord {
    def name: String = "def"
  }

  case object Else extends ReservedWord {
    def name: String = "else"

    override def canBeginStatement: Boolean = false
  }

  case object For extends ReservedWord {
    def name: String = "for"
  }

  case object If extends ReservedWord {
    def name: String = "if"
  }

  case object Lazy extends ReservedWord {
    def name: String = "lazy"
  }

  case object Match extends ReservedWord {
    def name: String = "match"

    override def canBeginStatement: Boolean = false
  }

  case object Val extends ReservedWord {
    def name: String = "val"
  }

  case object Var extends ReservedWord {
    def name: String = "var"
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

  case class BeginInterpolatedString(interpolatorName: String) extends Token

  case class BeginMultiLineInterpolatedString(interpolatorName: String) extends Token

  case class InterpolatedPart(value: String) extends Token

  case object BeginInterpolatedEscape extends Token

  case object EndInterpolatedEscape extends Token

  case object EndInterpolatedString extends Token

  sealed trait Identifier extends Token {
    override def canTerminateStatement: Boolean = true
  }

  object Identifier {
    case class Upper(name: String) extends Token with Identifier

    case class Lower(name: String) extends Token with Identifier

    case class Quoted(name: String) extends Token with Identifier

    case class Operator(name: String) extends Token with Identifier
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
  }

  case object Comma extends Token {
    override def canBeginStatement: Boolean = false
  }

  case object Newline extends Token

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

  case object Hash extends ReservedWord {
    def name: String = "#"

    override def canBeginStatement: Boolean = false
  }

  case object At extends ReservedWord {
    def name: String = "@"
  }

  val allReservedWords: Vector[ReservedWord] =
    Vector(At, Case, Colon, Def, Else, Eq, False, For, Hash, If, LArrow, Lazy, Match, Null, RDoubleArrow,
      True, Underscore, Val, Var, Yield)

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
