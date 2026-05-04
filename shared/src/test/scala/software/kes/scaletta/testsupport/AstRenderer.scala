package software.kes.scaletta.testsupport

import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.parser.{BindingPower, Operators}
import software.kes.scaletta.internal.scanner.Token
import software.kes.scaletta.testsupport.AstRenderer.Settings
import software.kes.scaletta.util.functional.Id.Id

object AstRenderer {
  def render(expression: Expression[Id],
             settings: Settings = Settings()): String = {
    val output = new StringBuilder()
    val renderer = new AstRenderer(settings, s => output.append(s))
    renderer.render(expression)
    output.result()
  }

  /**
   * Configuration settings for the [[AstRenderer]].
   *
   * @param indentSize           The number of spaces to use for each indentation level. Defaults to 2.
   *                             Only used when `compact` is false.
   * @param compact              If true, the output will be rendered on a single line with minimal spacing.
   *                             If false, the output will be pretty-printed with newlines and indentation.
   * @param parenthesizeAllCalls If true, all standard function calls will be wrapped in parentheses for clarity,
   *                             even when not strictly required by operator precedence.
   */
  case class Settings(indentSize: Int = 2,
                      compact: Boolean = false,
                      parenthesizeAllCalls: Boolean = false)
}

private class AstRenderer(settings: Settings,
                          write: String => Unit) {

  private var currentIndent: Int = 0

  private def indent(): Unit = {
    currentIndent += settings.indentSize
  }

  private def dedent(): Unit = {
    currentIndent -= settings.indentSize
  }

  private def writeIndent(): Unit = {
    if (!settings.compact) {
      write(" " * currentIndent)
    }
  }

  private def newline(): Unit = {
    if (!settings.compact) {
      write("\n")
    } else {
      write(" ")
    }
  }

  def render(expression: Expression[Id]): Unit = {
    renderWithPrecedence(expression, BindingPower.Minimum)
  }

  private def renderWithPrecedence(expression: Expression[Id],
                                   parentPrecedence: BindingPower): Unit = {
    expression match {
      case b: Block[Id] =>
        renderBlock(b)
      case Reference(id) =>
        write(id.name)
      case Select(qualifier, name) =>
        renderWithPrecedence(qualifier, BindingPower.MemberAccess)
        write(".")
        write(name.name)
      case Typed(expr, ascription) =>
        write("(")
        render(expr)
        write(": ")
        renderType(ascription)
        write(")")
      case lit: Literal[Id] =>
        renderLiteral(lit)
      case Tuple(elements) =>
        write("(")
        renderCommaSeparated(elements)
        write(")")
      case Conditional(condition, thenBranch, elseBranch) =>
        write("if (")
        render(condition)
        write(") ")
        render(thenBranch)
        write(" else ")
        render(elseBranch)
      case Call.Standard(target, typeArgs, args) =>
        if (settings.parenthesizeAllCalls) write("(")
        renderWithPrecedence(target, BindingPower.MemberAccess)
        if (settings.parenthesizeAllCalls) write(")")
        if (typeArgs.nonEmpty) {
          write("[")
          renderCommaSeparated(typeArgs.map(_.typ), renderType)
          write("]")
        }
        args.foreach(renderArgumentGroup)
      case Call.Infix(left, operation, typeArgs, right) =>
        val opToken = Token.Identifier.Operator(operation.name)
        val opPrecedence = Operators.bindingPower(opToken)
        val needsParens = opPrecedence < parentPrecedence
        if (needsParens) write("(")
        renderWithPrecedence(left, opPrecedence)
        write(" ")
        write(operation.name)
        if (typeArgs.nonEmpty) {
          write("[")
          renderCommaSeparated(typeArgs.map(_.typ), renderType)
          write("]")
        }
        write(" ")
        renderWithPrecedence(right, opPrecedence.nudge(1))
        if (needsParens) write(")")
      case Lambda(params, body) =>
        write("(")
        renderCommaSeparated(params, renderLambdaParameter)
        write(") => ")
        render(body)
      case InterpolatedString(interpolator, initial, segments) =>
        renderInterpolatedString(interpolator, initial, segments)
      case Match(expression, cases) =>
        renderWithPrecedence(expression, BindingPower.Minimum)
        write(" match {")
        indent()
        cases.foreach { c =>
          newline()
          writeIndent()
          write("case ")
          renderPattern(c.pattern)
          c.guard.foreach { g =>
            write(" if ")
            render(g)
          }
          write(" => ")
          render(c.body)
        }
        dedent()
        newline()
        writeIndent()
        write("}")
    }
  }

  private def renderBlock(block: Block[Id]): Unit = {
    write("{")
    if (block.declarations.nonEmpty) {
      indent()
      block.declarations.foreach { decl =>
        newline()
        writeIndent()
        renderDeclaration(decl)
      }
      newline()
      writeIndent()
      render(block.result)
      dedent()
      newline()
      writeIndent()
    } else {
      if (settings.compact) {
        write(" ")
        render(block.result)
        write(" ")
      } else {
        indent()
        newline()
        writeIndent()
        render(block.result)
        dedent()
        newline()
        writeIndent()
      }
    }
    write("}")
  }

  private def renderLiteral(lit: Literal[Id]): Unit = lit match {
    case Literal.IntLiteral(v) => write(v.toString)
    case Literal.LongLiteral(v) => write(v.toString + "L")
    case Literal.FloatLiteral(v) => write(v.toString + "f")
    case Literal.DoubleLiteral(v) => write(v.toString)
    case _: Literal.True[Id] => write("true")
    case _: Literal.False[Id] => write("false")
    case _: Literal.Null[Id] => write("null")
    case Literal.CharLiteral(v) => write("'" + escapeChar(v) + "'")
    case Literal.StringLiteral(v) => write("\"" + escapeString(v) + "\"")
  }

  private def renderType(ti: TypeIdentifier[Id]): Unit = ti match {
    case TypeIdentifier.Name(id) => write(id.name)
    case TypeIdentifier.Select(qualifier, id) =>
      renderType(qualifier)
      write(".")
      write(id.name)
    case TypeIdentifier.Applied(qualifier, args) =>
      renderType(qualifier)
      write("[")
      renderCommaSeparated(args, renderType)
      write("]")
    case TypeIdentifier.Function(params, result) =>
      if (params.size == 1) {
        renderType(params.head)
      } else {
        write("(")
        renderCommaSeparated(params, renderType)
        write(")")
      }
      write(" => ")
      renderType(result)
    case c: TypeIdentifier.Conjunction[Id] =>
      var first = true
      c.components.foreach { component =>
        if (!first) write(s" ${c.conjunctionType.operator} ")
        val comp = component
        comp match {
          case f: TypeIdentifier.Function[Id] =>
            write("(")
            renderType(f)
            write(")")
          case conj: TypeIdentifier.Conjunction[Id] =>
            write("(")
            renderType(conj)
            write(")")
          case other =>
            renderType(other)
        }
        first = false
      }
  }

  private def renderDeclaration(decl: Declaration[Id]): Unit = decl match {
    case Declaration.Val(pat, rhs) =>
      write("val ")
      renderPattern(pat)
      write(" = ")
      render(rhs)
    case Declaration.LazyVal(pat, rhs) =>
      write("lazy val ")
      renderPattern(pat)
      write(" = ")
      render(rhs)
    case Declaration.Def(name, params, returnType, body) =>
      write("def ")
      write(name.name)
      params.foreach(p => renderFormalParameterGroup(p))
      returnType.foreach { rt =>
        write(": ")
        renderType(rt)
      }
      write(" = ")
      render(body)
  }

  private def renderPattern(pat: Pattern[Id]): Unit = pat match {
    case Pattern.Identifier(name) => write(name.name)
    case _: Pattern.Wildcard[Id] => write("_")
    case Pattern.Literal(lit) => renderLiteral(lit)
    case Pattern.As(name, p) =>
      write(name.name)
      write(" @ ")
      renderPattern(p)
    case Pattern.Typed(p, ascription) =>
      renderPattern(p)
      write(": ")
      renderType(ascription)
    case Pattern.Tuple(elements) =>
      write("(")
      renderCommaSeparated(elements, renderPattern)
      write(")")
    case Pattern.Product(typeId, args) =>
      renderType(typeId)
      write("(")
      renderCommaSeparated(args, renderPattern)
      write(")")
  }

  private def renderArgumentGroup(ag: ArgumentGroup[Id]): Unit = {
    write("(")
    renderCommaSeparated(ag.arguments, renderArgument)
    ag.splat.foreach { s =>
      if (ag.arguments.nonEmpty) write(", ")
      renderArgument(s)
      write("*")
    }
    write(")")
  }

  private def renderArgument(arg: Argument[Id]): Unit = {
    arg.name.foreach { n =>
      write(n.name)
      write(" = ")
    }
    render(arg.value)
  }

  private def renderFormalParameterGroup(fpg: FormalParameterGroup[Id]): Unit = {
    write("(")
    renderCommaSeparated(fpg.parameters, renderFormalParameter)
    fpg.variadic.foreach { v =>
      if (fpg.parameters.nonEmpty) write(", ")
      renderFormalParameter(v)
      write("*")
    }
    write(")")
  }

  private def renderFormalParameter(fp: FormalParameter[Id]): Unit = {
    write(fp.name.name)
    write(": ")
    renderType(fp.typ)
    fp.default.foreach { d =>
      write(" = ")
      render(d)
    }
  }

  private def renderLambdaParameter(lp: LambdaParameter[Id]): Unit = {
    write(lp.name.name)
    lp.typ.foreach { t =>
      write(": ")
      renderType(t)
    }
  }

  private def renderInterpolatedString(interpolator: Interpolator,
                                       initial: String,
                                       segments: Vector[(Id[Expression[Id]], String)]): Unit = {
    write(interpolator.name)
    write("\"")
    write(initial) // TODO: escape? Interpolated strings might not need escaping of everything
    segments.foreach { case (expr, part) =>
      write("${")
      render(expr)
      write("}")
      write(part)
    }
    write("\"")
  }

  private def renderCommaSeparated[A](items: Seq[A], renderer: A => Unit = (a: A) => render(a.asInstanceOf[Expression[Id]])): Unit = {
    var first = true
    items.foreach { item =>
      if (!first) write(", ")
      renderer(item)
      first = false
    }
  }

  private def escapeChar(ch: Char): String = ch match {
    case '\'' => "\\'"
    case '\\' => "\\\\"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case _ if ch < ' ' || ch > '~' => "\\u%04x".format(ch.toInt)
    case _ => ch.toString
  }

  private def escapeString(s: String): String = {
    s.flatMap(escapeChar).replace("\"", "\\\"").replace("\\'", "'")
  }
}
