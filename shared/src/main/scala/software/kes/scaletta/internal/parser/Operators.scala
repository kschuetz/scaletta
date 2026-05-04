package software.kes.scaletta.internal.parser

import software.kes.scaletta.internal.scanner.{CharacterClass, Token}

object Operators {
  def bindingPower(identifier: Token.Identifier): BindingPower = {
    identifier match {
      case _: Token.Identifier.Operator => bindingPowerByName(identifier.name)
      case _ => BindingPower.Alphanumeric
    }
  }

  def bindingPower(reservedWord: Token.ReservedWord): BindingPower = {
    reservedWord match {
      case Token.Colon => BindingPower.Ascription
      case Token.Pipe => BindingPower.LogicalOr
      case Token.Ampersand => BindingPower.LogicalAnd
      case Token.Eq => BindingPower.Minimum
      case _ => bindingPowerByName(reservedWord.name)
    }
  }

  private def bindingPowerByName(name: String): BindingPower = {
    if (name.isEmpty) {
      BindingPower.Alphanumeric
    } else {
      (name.head: @scala.annotation.switch) match {
        case '|' => BindingPower.LogicalOr
        case '^' => BindingPower.LogicalXor
        case '&' => BindingPower.LogicalAnd
        case '<' | '>' => BindingPower.Comparison
        case '!' => BindingPower.Equality
        case '=' => if (name.length > 1) BindingPower.Equality else BindingPower.Minimum
        case ':' => BindingPower.ColonOperator
        case '+' | '-' => BindingPower.Addition
        case '*' | '/' | '%' => BindingPower.Multiplication
        case ch if CharacterClass.isOperator(ch) => BindingPower.OtherSymbolicOperators
        case _ => BindingPower.Alphanumeric
      }
    }
  }
}
