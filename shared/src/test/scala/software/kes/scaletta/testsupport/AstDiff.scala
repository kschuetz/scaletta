package software.kes.scaletta.testsupport

import software.kes.scaletta.internal.ast.{Call, Expression, Literal, Reference}
import software.kes.scaletta.util.functional.Id._

object AstDiff {

  /**
   * Performs a structural comparison of two expressions and returns a descriptive
   * mismatch message if they differ.
   *
   * @param actual   the actual expression produced by the parser
   * @param expected the expected expression
   * @param path     the path to the current node being compared (for error reporting)
   * @return Some(mismatchMessage) if they differ, None if they are equal
   */
  def diff(actual: Expression[Id], expected: Expression[Id], path: String = "root"): Option[String] = {
    if (actual == expected) None
    else {
      if (actual.getClass == expected.getClass) {
        (actual: Any, expected: Any) match {
          case (a: Call.Standard[_], e: Call.Standard[_]) =>
            diff(a.target.asInstanceOf[Expression[Id]], e.target.asInstanceOf[Expression[Id]], s"$path -> target")
              .orElse {
                if (a.args.length != e.args.length) {
                  Some(s"$path -> args: group length mismatch (actual: ${a.args.length}, expected: ${e.args.length})")
                } else {
                  a.args.zip(e.args).zipWithIndex.collectFirst {
                    case ((aaGroup, eeGroup), i) =>
                      val actualGroup = aaGroup.asInstanceOf[software.kes.scaletta.internal.ast.ArgumentGroup[Id]]
                      val expectedGroup = eeGroup.asInstanceOf[software.kes.scaletta.internal.ast.ArgumentGroup[Id]]
                      diffArgumentGroup(actualGroup, expectedGroup, s"$path -> args($i)")
                  }.flatten
                }
              }
          case (a: Call.Infix[_], e: Call.Infix[_]) =>
            if (a.operation.asInstanceOf[Id[software.kes.scaletta.internal.ast.Identifier[Id]]] != e.operation.asInstanceOf[Id[software.kes.scaletta.internal.ast.Identifier[Id]]]) {
              Some(s"$path -> operation: mismatch (actual: ${a.operation}, expected: ${e.operation})")
            } else {
              diff(a.left.asInstanceOf[Expression[Id]], e.left.asInstanceOf[Expression[Id]], s"$path -> left")
                .orElse(diff(a.right.asInstanceOf[Expression[Id]], e.right.asInstanceOf[Expression[Id]], s"$path -> right"))
            }
          case (a: Literal.IntLiteral[_], e: Literal.IntLiteral[_]) =>
            if (a.value != e.value) Some(s"$path: value mismatch (actual: ${a.value}, expected: ${e.value})")
            else None
          case (a: Reference[_], e: Reference[_]) =>
            val actualId = a.id.asInstanceOf[Id[software.kes.scaletta.internal.ast.Identifier[Id]]]
            val expectedId = e.id.asInstanceOf[Id[software.kes.scaletta.internal.ast.Identifier[Id]]]
            if (actualId.name != expectedId.name) Some(s"$path: name mismatch (actual: ${actualId.name}, expected: ${expectedId.name})")
            else None
          case _ => Some(s"$path: mismatch\n  Actual:   $actual\n  Expected: $expected")
        }
      } else {
        Some(s"$path: type mismatch (actual: ${actual.getClass.getSimpleName}, expected: ${expected.getClass.getSimpleName})")
      }
    }
  }

  private def diffArgumentGroup(actual: software.kes.scaletta.internal.ast.ArgumentGroup[Id],
                                expected: software.kes.scaletta.internal.ast.ArgumentGroup[Id],
                                path: String): Option[String] = {
    if (actual.arguments.length != expected.arguments.length) {
      Some(s"$path: argument length mismatch (actual: ${actual.arguments.length}, expected: ${expected.arguments.length})")
    } else {
      actual.arguments.zip(expected.arguments).zipWithIndex.collectFirst {
        case ((aaArg, eeArg), i) =>
          val actualArg = aaArg
          val expectedArg = eeArg
          diff(actualArg.value, expectedArg.value, s"$path($i)")
      }.flatten
    }
  }
}
