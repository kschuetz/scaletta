package software.kes.scaletta.api

import software.kes.scaletta.symbols.QualifiedName
import software.kes.scaletta.types.{Type, TypeId}

trait Module[A] {
  def configure(setup: Setup): A

  def map[B](fn: A => B): Module[B] =
    Module.mapped(this, fn)

  def flatMap[B](fn: A => Module[B]): Module[B] =
    Module.flatMapped(this, fn)

  def as[B](value: => B): Module[B] =
    Module.mapped[A, B](this, _ => value)

  def zip[B](rhs: Module[B]): Module[(A, B)] =
    Module.zip(this, rhs)

  def unit: Module[Unit] =
    Module.mapped[A, Unit](this, _ => ())

  def flatten[B](implicit ev: A <:< Module[B]): Module[B] =
    flatMap(ev)

  /**
   * Performs a side-effect with the result of the registration, while keeping the original result.
   *
   * This is useful for "intercepting" registration results (such as TypeIds or NativeFunctionIds)
   * for logging, debugging, or notifying external systems, without breaking the module chain
   * or altering the return type.
   *
   * @param fn the side-effect to perform
   * @return a new Module that yields the same result as this one
   */
  def tap(fn: A => Unit): Module[A] =
    map { a =>
      fn(a)
      a
    }
}

object Module {
  /**
   * Create a module where anything can be registered
   */
  def apply[A](fn: Setup => A): Module[A] =
    new Basic(fn)

  val empty: Module[Unit] = pure(())

  /**
   * Creates a module that registers nothing but yields a constant value.
   */
  def pure[A](value: => A): Module[A] = Module { _ => value }

  /**
   * Create a module where only methods need to be registered
   */
  def methodsOnly[A](fn: MethodRegistry => A): Module[A] =
    Module { registry => fn(registry.methodRegistry) }

  /**
   * Create a module where only types need to be registered
   */
  def typesOnly[A](fn: TypeRegistry => A): Module[A] =
    Module { registry => fn(registry.typeRegistry) }

  /**
   * Create a module where only runtime contexts need to be registered
   */
  def runtimeContextsOnly[A](fn: RuntimeContextRegistry => A): Module[A] =
    Module { registry => fn(registry.runtimeContextRegistry) }

  /**
   * Create a module consisting of a single ref type
   */
  def refType(name: QualifiedName.Full): Module[Type.Nominal[TypeId]] =
    Module.typesOnly(_.addRefType(name))

  /**
   * Create a module that registers a new runtime context type
   */
  lazy val newRuntimeContext: Module[RuntimeContextId] =
    Module {
      _.runtimeContextRegistry.createRuntimeContextType()
    }

  /**
   * Declares that the underlying module has 'pureHint' set to the specified value
   * as a default.
   */
  def withPureHint[A](value: Boolean)
                     (underlying: Module[A]): Module[A] =
    withMethodRegistrySettings(_.withPureHint(value))(underlying)

  /**
   * Declares that the underlying module requires the specified runtime contexts
   * as a default.
   */
  def requireContexts[A](contexts: RuntimeContextId*)
                        (underlying: Module[A]): Module[A] =
    withMethodRegistrySettings(_.requireContexts(contexts: _*))(underlying)

  /**
   * Updates the default method registry settings for the underlying module
   */
  def withMethodRegistrySettings[A](fn: MethodRegistry.Settings => MethodRegistry.Settings)
                                   (underlying: Module[A]): Module[A] =
    Module { registry =>
      registry.methodRegistry.pushSettings(fn)
      val result = underlying.configure(registry)
      registry.methodRegistry.popSettings()
      result
    }

  def composite(modules: Module[_]*): Module[Unit] =
    fromSeq(modules)

  def fromSeq(modules: Seq[Module[_]]): Module[Unit] =
    new Composite(modules)

  def sequence[A](modules: Seq[Module[A]]): Module[Seq[A]] =
    traverse(modules)(identity)

  def when(condition: Boolean)(module: => Module[Unit]): Module[Unit] =
    if (condition) module else Module.empty

  def unless(condition: Boolean)(module: => Module[Unit]): Module[Unit] =
    if (!condition) module else Module.empty

  def traverse[A, B](items: Seq[A])(fn: A => Module[B]): Module[Seq[B]] =
    new Traversed(items, fn)

  def combine[A, Result](ma: Module[A])
                        (fn: A => Result): Module[Result] =
    ma.map(fn)

  def combine[A, B, Result](ma: Module[A],
                            mb: Module[B])
                           (fn: (A, B) => Result): Module[Result] =
    new Zipped2(ma, mb, fn)

  def combine[A, B, C, Result](ma: Module[A],
                               mb: Module[B],
                               mc: Module[C])
                              (fn: (A, B, C) => Result): Module[Result] =
    new Zipped3(ma, mb, mc, fn)

  def combine[A, B, C, D, Result](ma: Module[A],
                                  mb: Module[B],
                                  mc: Module[C],
                                  md: Module[D])
                                 (fn: (A, B, C, D) => Result): Module[Result] =
    new Zipped4(ma, mb, mc, md, fn)

  def combine[A, B, C, D, E, Result](ma: Module[A],
                                     mb: Module[B],
                                     mc: Module[C],
                                     md: Module[D],
                                     me: Module[E])
                                    (fn: (A, B, C, D, E) => Result): Module[Result] =
    new Zipped5(ma, mb, mc, md, me, fn)

  def combine[A, B, C, D, E, F, Result](ma: Module[A],
                                        mb: Module[B],
                                        mc: Module[C],
                                        md: Module[D],
                                        me: Module[E],
                                        mf: Module[F])
                                       (fn: (A, B, C, D, E, F) => Result): Module[Result] =
    new Zipped6(ma, mb, mc, md, me, mf, fn)

  def combine[A, B, C, D, E, F, G, Result](ma: Module[A],
                                           mb: Module[B],
                                           mc: Module[C],
                                           md: Module[D],
                                           me: Module[E],
                                           mf: Module[F],
                                           mg: Module[G])
                                          (fn: (A, B, C, D, E, F, G) => Result): Module[Result] =
    new Zipped7(ma, mb, mc, md, me, mf, mg, fn)

  def combine[A, B, C, D, E, F, G, H, Result](ma: Module[A],
                                              mb: Module[B],
                                              mc: Module[C],
                                              md: Module[D],
                                              me: Module[E],
                                              mf: Module[F],
                                              mg: Module[G],
                                              mh: Module[H])
                                             (fn: (A, B, C, D, E, F, G, H) => Result): Module[Result] =
    new Zipped8(ma, mb, mc, md, me, mf, mg, mh, fn)

  def combine[A, B, C, D, E, F, G, H, I, Result](ma: Module[A],
                                                 mb: Module[B],
                                                 mc: Module[C],
                                                 md: Module[D],
                                                 me: Module[E],
                                                 mf: Module[F],
                                                 mg: Module[G],
                                                 mh: Module[H],
                                                 mi: Module[I])
                                                (fn: (A, B, C, D, E, F, G, H, I) => Result): Module[Result] =
    new Zipped9(ma, mb, mc, md, me, mf, mg, mh, mi, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, Result](ma: Module[A],
                                                    mb: Module[B],
                                                    mc: Module[C],
                                                    md: Module[D],
                                                    me: Module[E],
                                                    mf: Module[F],
                                                    mg: Module[G],
                                                    mh: Module[H],
                                                    mi: Module[I],
                                                    mj: Module[J])
                                                   (fn: (A, B, C, D, E, F, G, H, I, J) => Result): Module[Result] =
    new Zipped10(ma, mb, mc, md, me, mf, mg, mh, mi, mj, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, K, Result](ma: Module[A],
                                                       mb: Module[B],
                                                       mc: Module[C],
                                                       md: Module[D],
                                                       me: Module[E],
                                                       mf: Module[F],
                                                       mg: Module[G],
                                                       mh: Module[H],
                                                       mi: Module[I],
                                                       mj: Module[J],
                                                       mk: Module[K])
                                                      (fn: (A, B, C, D, E, F, G, H, I, J, K) => Result): Module[Result] =
    new Zipped11(ma, mb, mc, md, me, mf, mg, mh, mi, mj, mk, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, K, L, Result](ma: Module[A],
                                                          mb: Module[B],
                                                          mc: Module[C],
                                                          md: Module[D],
                                                          me: Module[E],
                                                          mf: Module[F],
                                                          mg: Module[G],
                                                          mh: Module[H],
                                                          mi: Module[I],
                                                          mj: Module[J],
                                                          mk: Module[K],
                                                          ml: Module[L])
                                                         (fn: (A, B, C, D, E, F, G, H, I, J, K, L) => Result): Module[Result] =
    new Zipped12(ma, mb, mc, md, me, mf, mg, mh, mi, mj, mk, ml, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, K, L, M, Result](ma: Module[A],
                                                             mb: Module[B],
                                                             mc: Module[C],
                                                             md: Module[D],
                                                             me: Module[E],
                                                             mf: Module[F],
                                                             mg: Module[G],
                                                             mh: Module[H],
                                                             mi: Module[I],
                                                             mj: Module[J],
                                                             mk: Module[K],
                                                             ml: Module[L],
                                                             mm: Module[M])
                                                            (fn: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Result): Module[Result] =
    new Zipped13(ma, mb, mc, md, me, mf, mg, mh, mi, mj, mk, ml, mm, fn)

  private final class Zipped13[A, B, C, D, E, F, G, H, I, J, K, L, M, Result](ma: Module[A],
                                                                              mb: Module[B],
                                                                              mc: Module[C],
                                                                              md: Module[D],
                                                                              me: Module[E],
                                                                              mf: Module[F],
                                                                              mg: Module[G],
                                                                              mh: Module[H],
                                                                              mi: Module[I],
                                                                              mj: Module[J],
                                                                              mk: Module[K],
                                                                              ml: Module[L],
                                                                              mm: Module[M],
                                                                              fn: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      val g = mg.configure(setup)
      val h = mh.configure(setup)
      val i = mi.configure(setup)
      val j = mj.configure(setup)
      val k = mk.configure(setup)
      val l = ml.configure(setup)
      val m = mm.configure(setup)
      fn(a, b, c, d, e, f, g, h, i, j, k, l, m)
    }
  }

  private def mapped[A, B](underlying: Module[A],
                           fn: A => B): Module[B] =
    new Mapped[A, B](underlying, fn)

  private def flatMapped[A, B](underlying: Module[A],
                               fn: A => Module[B]): Module[B] =
    new FlatMapped[A, B](underlying, fn)

  private def zip[A, B](ma: Module[A], mb: Module[B]): Module[(A, B)] =
    new Zipped2(ma, mb, (a: A, b: B) => (a, b))

  private class Basic[A](fn: Setup => A) extends Module[A] {
    def configure(setup: Setup): A = fn(setup)
  }


  private class Mapped[A, B](underlying: Module[A],
                             fn: A => B) extends Module[B] {
    def configure(setup: Setup): B =
      fn(underlying.configure(setup))
  }

  private class FlatMapped[A, B](underlying: Module[A],
                                 fn: A => Module[B]) extends Module[B] {
    def configure(setup: Setup): B =
      fn(underlying.configure(setup)).configure(setup)
  }

  private class Composite(components: Seq[Module[_]]) extends Module[Unit] {
    def configure(setup: Setup): Unit = {
      components.foreach(_.configure(setup))
    }
  }

  private final class Traversed[A, B](items: Seq[A],
                                      fn: A => Module[B]) extends Module[Seq[B]] {
    def configure(setup: Setup): Seq[B] = {
      items.foldLeft(Vector.empty[B]) { (acc, item) =>
        acc :+ fn(item).configure(setup)
      }
    }
  }

  private final class Zipped2[A, B, Result](ma: Module[A],
                                            mb: Module[B],
                                            fn: (A, B) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      fn(a, b)
    }
  }

  private final class Zipped3[A, B, C, Result](ma: Module[A],
                                               mb: Module[B],
                                               mc: Module[C],
                                               fn: (A, B, C) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      fn(a, b, c)
    }
  }

  private final class Zipped4[A, B, C, D, Result](ma: Module[A],
                                                  mb: Module[B],
                                                  mc: Module[C],
                                                  md: Module[D],
                                                  fn: (A, B, C, D) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      fn(a, b, c, d)
    }
  }

  private final class Zipped5[A, B, C, D, E, Result](ma: Module[A],
                                                     mb: Module[B],
                                                     mc: Module[C],
                                                     md: Module[D],
                                                     me: Module[E],
                                                     fn: (A, B, C, D, E) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      fn(a, b, c, d, e)
    }
  }

  private final class Zipped6[A, B, C, D, E, F, Result](ma: Module[A],
                                                        mb: Module[B],
                                                        mc: Module[C],
                                                        md: Module[D],
                                                        me: Module[E],
                                                        mf: Module[F],
                                                        fn: (A, B, C, D, E, F) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      fn(a, b, c, d, e, f)
    }
  }

  private final class Zipped7[A, B, C, D, E, F, G, Result](ma: Module[A],
                                                           mb: Module[B],
                                                           mc: Module[C],
                                                           md: Module[D],
                                                           me: Module[E],
                                                           mf: Module[F],
                                                           mg: Module[G],
                                                           fn: (A, B, C, D, E, F, G) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      val g = mg.configure(setup)
      fn(a, b, c, d, e, f, g)
    }
  }

  private final class Zipped8[A, B, C, D, E, F, G, H, Result](ma: Module[A],
                                                              mb: Module[B],
                                                              mc: Module[C],
                                                              md: Module[D],
                                                              me: Module[E],
                                                              mf: Module[F],
                                                              mg: Module[G],
                                                              mh: Module[H],
                                                              fn: (A, B, C, D, E, F, G, H) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      val g = mg.configure(setup)
      val h = mh.configure(setup)
      fn(a, b, c, d, e, f, g, h)
    }
  }

  private final class Zipped9[A, B, C, D, E, F, G, H, I, Result](ma: Module[A],
                                                                 mb: Module[B],
                                                                 mc: Module[C],
                                                                 md: Module[D],
                                                                 me: Module[E],
                                                                 mf: Module[F],
                                                                 mg: Module[G],
                                                                 mh: Module[H],
                                                                 mi: Module[I],
                                                                 fn: (A, B, C, D, E, F, G, H, I) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      val g = mg.configure(setup)
      val h = mh.configure(setup)
      val i = mi.configure(setup)
      fn(a, b, c, d, e, f, g, h, i)
    }
  }

  private final class Zipped10[A, B, C, D, E, F, G, H, I, J, Result](ma: Module[A],
                                                                     mb: Module[B],
                                                                     mc: Module[C],
                                                                     md: Module[D],
                                                                     me: Module[E],
                                                                     mf: Module[F],
                                                                     mg: Module[G],
                                                                     mh: Module[H],
                                                                     mi: Module[I],
                                                                     mj: Module[J],
                                                                     fn: (A, B, C, D, E, F, G, H, I, J) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      val g = mg.configure(setup)
      val h = mh.configure(setup)
      val i = mi.configure(setup)
      val j = mj.configure(setup)
      fn(a, b, c, d, e, f, g, h, i, j)
    }
  }

  private final class Zipped11[A, B, C, D, E, F, G, H, I, J, K, Result](ma: Module[A],
                                                                        mb: Module[B],
                                                                        mc: Module[C],
                                                                        md: Module[D],
                                                                        me: Module[E],
                                                                        mf: Module[F],
                                                                        mg: Module[G],
                                                                        mh: Module[H],
                                                                        mi: Module[I],
                                                                        mj: Module[J],
                                                                        mk: Module[K],
                                                                        fn: (A, B, C, D, E, F, G, H, I, J, K) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      val g = mg.configure(setup)
      val h = mh.configure(setup)
      val i = mi.configure(setup)
      val j = mj.configure(setup)
      val k = mk.configure(setup)
      fn(a, b, c, d, e, f, g, h, i, j, k)
    }
  }

  private final class Zipped12[A, B, C, D, E, F, G, H, I, J, K, L, Result](ma: Module[A],
                                                                           mb: Module[B],
                                                                           mc: Module[C],
                                                                           md: Module[D],
                                                                           me: Module[E],
                                                                           mf: Module[F],
                                                                           mg: Module[G],
                                                                           mh: Module[H],
                                                                           mi: Module[I],
                                                                           mj: Module[J],
                                                                           mk: Module[K],
                                                                           ml: Module[L],
                                                                           fn: (A, B, C, D, E, F, G, H, I, J, K, L) => Result) extends Module[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      val f = mf.configure(setup)
      val g = mg.configure(setup)
      val h = mh.configure(setup)
      val i = mi.configure(setup)
      val j = mj.configure(setup)
      val k = mk.configure(setup)
      val l = ml.configure(setup)
      fn(a, b, c, d, e, f, g, h, i, j, k, l)
    }
  }
}
