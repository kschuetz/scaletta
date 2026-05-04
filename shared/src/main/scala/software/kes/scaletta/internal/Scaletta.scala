package software.kes.scaletta.internal

import software.kes.scaletta.api
import software.kes.scaletta.api.{ScalettaModule, Settings}

object Scaletta {

  def builder: Builder = {
    val settings = Settings()
    new Builder(settings, Nil)
  }

  final class Builder private[Scaletta](val settings: Settings,
                                        private val modules: List[ScalettaModule[Unit]])
    extends api.Scaletta.Builder {

    def addModule[A](modulesToAdd: ScalettaModule[A]*): Builder =
      if (modules.isEmpty) this
      else new Builder(settings, modulesToAdd.toList.map(_.unit) ++ modules)

    def modifySettings(fns: Settings => Settings*): Builder = {
      val updatedSettings = fns.foldLeft(settings) {
        case (acc, fn) => fn(acc)
      }
      new Builder(updatedSettings, modules)
    }

    def build: api.Scaletta = new Scaletta
  }

}

final class Scaletta extends api.Scaletta {

}
