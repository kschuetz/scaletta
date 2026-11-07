package software.kes.scaletta.internal.semantics

import scala.annotation.tailrec

object TypeEnvironment {
  val empty: TypeEnvironment = TypeEnvironment(Nil)
}

final case class TypeEnvironment(layers: List[Vector[TypedScopeBinding]]) {

  def pushLayer(layer: Vector[TypedScopeBinding]): TypeEnvironment =
    TypeEnvironment(layer :: layers)

  def extendCurrentLayer(binding: TypedScopeBinding): TypeEnvironment =
    layers match {
      case Nil => TypeEnvironment(Vector(binding) :: Nil)
      case current :: rest => TypeEnvironment((current :+ binding) :: rest)
    }

  def lookup(name: String): Option[(Int, TypedScopeBinding)] = {
    @tailrec
    def go(remaining: List[Vector[TypedScopeBinding]], depth: Int): Option[(Int, TypedScopeBinding)] = {
      remaining match {
        case Nil => None
        case current :: rest =>
          val found = current.foldLeft(Option.empty[TypedScopeBinding]) { (acc, binding) =>
            acc match {
              case Some(_) => acc
              case None =>
                if (binding.name == name) {
                  Some(binding)
                } else {
                  None
                }
            }
          }
          found match {
            case Some(binding) => Some((depth, binding))
            case None => go(rest, depth + 1)
          }
      }
    }

    go(layers, 0)
  }
}
