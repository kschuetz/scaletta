package software.kes.scaletta.testsupport

import software.kes.scaletta.ast.{Call, Expression, Literal, Reference}
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
                  Some(s"$path -> args: length mismatch (actual: ${a.args.length}, expected: ${e.args.length})")
                } else {
                  a.args.zip(e.args).zipWithIndex.collectFirst {
                    case ((aa, ee), i) if aa != ee =>
                      val actualArg = aa.asInstanceOf[Expression[Id]]
                      val expectedArg = ee.asInstanceOf[Expression[Id]]
                      diff(actualArg, expectedArg, s"$path -> args($i)").getOrElse(s"$path -> args($i) differs")
                  }
                }
              }
          case (a: Call.Infix[_], e: Call.Infix[_]) =>
            if (a.operation.asInstanceOf[Id[software.kes.scaletta.ast.Identifier[Id]]] != e.operation.asInstanceOf[Id[software.kes.scaletta.ast.Identifier[Id]]]) {
              Some(s"$path -> operation: mismatch (actual: ${a.operation}, expected: ${e.operation})")
            } else {
              diff(a.left.asInstanceOf[Expression[Id]], e.left.asInstanceOf[Expression[Id]], s"$path -> left")
                .orElse(diff(a.right.asInstanceOf[Expression[Id]], e.right.asInstanceOf[Expression[Id]], s"$path -> right"))
            }
          case (a: Literal.IntLiteral[_], e: Literal.IntLiteral[_]) =>
            if (a.value != e.value) Some(s"$path: value mismatch (actual: ${a.value}, expected: ${e.value})")
            else None
          case (a: Reference[_], e: Reference[_]) =>
            val actualId = a.id.asInstanceOf[Id[software.kes.scaletta.ast.Identifier[Id]]]
            val expectedId = e.id.asInstanceOf[Id[software.kes.scaletta.ast.Identifier[Id]]]
            if (actualId.name != expectedId.name) Some(s"$path: name mismatch (actual: ${actualId.name}, expected: ${expectedId.name})")
            else None
          case _ => Some(s"$path: mismatch\n  Actual:   $actual\n  Expected: $expected")
        }
      } else {
        Some(s"$path: type mismatch (actual: ${actual.getClass.getSimpleName}, expected: ${expected.getClass.getSimpleName})")
      }
    }
  }
}
