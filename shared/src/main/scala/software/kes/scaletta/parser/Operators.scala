package software.kes.scaletta.parser

import software.kes.scaletta.scanner.{CharacterClass, Token}

object Operators {
  def bindingPower(identifier: Token.Identifier): BindingPower = {
    identifier match {
      case _: Token.Identifier.Operator => bindingPowerByName(identifier.name)
      case _ => BindingPower.Alphanumeric
    }
  }

  def bindingPower(reservedWord: Token.ReservedWord): BindingPower = {
    reservedWord match {
      case Token.Colon => BindingPower.Colon
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
        case '!' | '=' => BindingPower.Equality
        case ':' => BindingPower.Colon
        case '+' | '-' => BindingPower.Addition
        case '*' | '/' | '%' => BindingPower.Multiplication
        case ch if CharacterClass.isOperator(ch) => BindingPower.AllOthers
        case _ => BindingPower.Alphanumeric
      }
    }
  }
}
