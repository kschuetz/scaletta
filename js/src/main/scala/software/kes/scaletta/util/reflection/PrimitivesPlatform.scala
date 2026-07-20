package software.kes.scaletta.util.reflection

import scala.scalajs.js

private[reflection] object PrimitivesPlatform {
  def isAnyVal(value: Any): Boolean = {
    if (value == null) return false

    // Fast path using pattern matching (works with Scala.js numeric encoding and booleans/chars)
    value match {
      case _: Boolean => true
      case _: Byte => true
      case _: Short => true
      case _: Int => true
      case _: Long => true // Scala.js Long is a value type with runtime Long impl
      case _: Char => true // encoded as Int in Scala.js, but pattern still matches
      case _: Float => true
      case _: Double => true
      case _: Unit => true
      case _ =>
        // Fallback: inspect JS types to cover edge cases
        val anyRef = value.asInstanceOf[AnyRef]
        // Check boxed Unit
        if (anyRef.getClass.getName == "scala.runtime.BoxedUnit") return true

        // JS-level checks (booleans and numbers)
        if (js.typeOf(value.asInstanceOf[Any]) == "boolean") return true
        if (js.typeOf(value.asInstanceOf[Any]) == "number" && !js.isUndefined(value)) return true

        false
    }
  }
}
