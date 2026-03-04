package software.kes.scaletta.parser

import software.kes.scaletta.scanner.Token

object Operators {
  def bindingPower(identifier: Token.Identifier): BindingPower = {
    val name = identifier.name
    if (name.isEmpty) {
      BindingPower.AllOthers
    } else {
      name.head match {
        case '|' => BindingPower.LogicalOr
        case '^' => BindingPower.LogicalXor
        case '&' => BindingPower.LogicalAnd
        case '<' | '>' => BindingPower.Comparison
        case '!' | '=' => BindingPower.Equality
        case ':' => BindingPower.Colon
        case '+' | '-' => BindingPower.Addition
        case '*' | '/' | '%' => BindingPower.Multiplication
        case _ => BindingPower.AllOthers
      }
    }
  }
}
