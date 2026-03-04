package software.kes.scaletta.util.functional

/**
 * A basic natural transformation from F to G.
 */
trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
