package software.kes.scaletta.api

/**
 * @param impl function that accepts variable number of arguments (always at least one, and always odd).
 *             Args with even-numbered indices (e.g., 0, 2, 4) contain strings
 *             Args with odd-numbered indices contain the values of the interpolated variables (these are of type Any)
 */
case class CustomInterpolator(returnType: ProperType[TypeId],
                              impl: FunctionImpl)
