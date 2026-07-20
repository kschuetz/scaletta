package software.kes.scaletta.util.reflection

private[reflection] object PrimitivesPlatform {
  def isAnyVal(value: Any): Boolean = {
    if (value == null) false
    else value match {
      case _: Boolean => true
      case _: Byte => true
      case _: Short => true
      case _: Int => true
      case _: Long => true
      case _: Char => true
      case _: Float => true
      case _: Double => true
      case _: Unit => true // rarely seen, but match keeps symmetry with JS impl
      case _ =>
        // In case the value is boxed as a Java wrapper or BoxedUnit
        val c = value.asInstanceOf[AnyRef].getClass
        (c eq classOf[java.lang.Boolean]) ||
          (c eq classOf[java.lang.Byte]) ||
          (c eq classOf[java.lang.Short]) ||
          (c eq classOf[java.lang.Integer]) ||
          (c eq classOf[java.lang.Long]) ||
          (c eq classOf[java.lang.Character]) ||
          (c eq classOf[java.lang.Float]) ||
          (c eq classOf[java.lang.Double]) ||
          (c.getName == "scala.runtime.BoxedUnit")
    }
  }
}
