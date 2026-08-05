package software.kes.scaletta.util.functional

trait FunctorK[T[_[_]]] {
  def mapK[F[_], G[_]](tf: T[F])(nt: F ~> G)(implicit F: Functor[F]): T[G]
}
