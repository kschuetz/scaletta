package software.kes.scaletta.api

object Scaletta {

  def builder: Builder =
    software.kes.scaletta.internal.ScalettaFacade.builder

  trait Builder {
    /**
     * Adds Scaletta modules to the builder.
     */
    def addModule[A](modules: ScalettaModule[A]*): Builder

    /**
     * Modifies the settings of the builder.
     */
    def modifySettings(fns: Settings => Settings*): Builder

    def build: Scaletta
  }

  /**
   * Alternate method of creating a Scaletta instance.
   * Uses a builder to create a Scaletta instance by applying a sequence of modifiers.
   *
   * @param modifiers functions to modify the builder, run in sequence
   */
  def create(modifiers: Builder => Builder*): Scaletta =
    modifiers.foldLeft(builder) {
      case (acc, modifier) => modifier(acc)
    }.build

  /**
   * Modifier that can be passed to the [[create]] function.
   */
  def addModule[A](modules: ScalettaModule[A]*): Builder => Builder =
    _.addModule(modules: _*)
}

trait Scaletta
