package software.kes.scaletta.internal.semantics

import scala.annotation.tailrec

object LexicalScope {
  val empty: LexicalScope = LexicalScope(Nil)
}

final case class LexicalScope(layers: List[Vector[ScopeBinding]]) {

  def pushLayer(layer: Vector[ScopeBinding]): LexicalScope =
    LexicalScope(layer :: layers)

  def lookup(name: String): Option[(Int, ScopeBinding)] = {
    @tailrec
    def go(remaining: List[Vector[ScopeBinding]], depth: Int): Option[(Int, ScopeBinding)] = {
      remaining match {
        case Nil => None
        case current :: rest =>
          val found = current.foldLeft(Option.empty[ScopeBinding]) { (acc, binding) =>
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
