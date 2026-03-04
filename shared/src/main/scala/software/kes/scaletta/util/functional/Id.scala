package software.kes.scaletta.util.functional

object Id {
  type Id[A] = A

  implicit object idFunctor extends Functor[Id] {
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }
}
