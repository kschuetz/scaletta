package software.kes.scaletta.api

import software.kes.scaletta.internal.symbols.QualifiedName

trait ScalettaModule[A] {
  def configure(setup: Setup): A

  def map[B](fn: A => B): ScalettaModule[B] =
    ScalettaModule.mapped(this, fn)

  def flatMap[B](fn: A => ScalettaModule[B]): ScalettaModule[B] =
    ScalettaModule.flatMapped(this, fn)

  def as[B](value: => B): ScalettaModule[B] =
    ScalettaModule.mapped[A, B](this, _ => value)

  def zip[B](rhs: ScalettaModule[B]): ScalettaModule[(A, B)] =
    ScalettaModule.zip(this, rhs)

  def unit: ScalettaModule[Unit] =
    ScalettaModule.mapped[A, Unit](this, _ => ())

  def flatten[B](implicit ev: A <:< ScalettaModule[B]): ScalettaModule[B] =
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
  def tap(fn: A => Unit): ScalettaModule[A] =
    map { a =>
      fn(a)
      a
    }
}

object ScalettaModule {
  /**
   * Create a module where anything can be registered
   */
  def apply[A](fn: Setup => A): ScalettaModule[A] =
    new Basic(fn)

  val empty: ScalettaModule[Unit] = pure(())

  /**
   * Creates a module that registers nothing but yields a constant value.
   */
  def pure[A](value: => A): ScalettaModule[A] = ScalettaModule { _ => value }

  /**
   * Create a module where only methods need to be registered
   */
  def methodsOnly[A](fn: MethodRegistry => A): ScalettaModule[A] =
    ScalettaModule { registry => fn(registry.methodRegistry) }

  /**
   * Create a module where only types need to be registered
   */
  def typesOnly[A](fn: TypeRegistry => A): ScalettaModule[A] =
    ScalettaModule { registry => fn(registry.typeRegistry) }

  /**
   * Create a module where only runtime contexts need to be registered
   */
  def runtimeContextsOnly[A](fn: RuntimeContextRegistry => A): ScalettaModule[A] =
    ScalettaModule { registry => fn(registry.runtimeContextRegistry) }

  /**
   * Create a module consisting of a single ref type
   */
  def refType(name: QualifiedName.Full): ScalettaModule[Type.Nominal[TypeId]] =
    ScalettaModule.typesOnly(_.addRefType(name))

  /**
   * Create a module that registers a new runtime context type
   */
  lazy val newRuntimeContext: ScalettaModule[RuntimeContextId] =
    ScalettaModule {
      _.runtimeContextRegistry.createRuntimeContextType()
    }

  /**
   * Declares that the underlying module has 'pureHint' set to the specified value
   * as a default.
   */
  def withPureHint[A](value: Boolean)
                     (underlying: ScalettaModule[A]): ScalettaModule[A] =
    withMethodRegistrySettings(_.withPureHint(value))(underlying)

  /**
   * Declares that the underlying module requires the specified runtime contexts
   * as a default.
   */
  def requireContexts[A](contexts: RuntimeContextId*)
                        (underlying: ScalettaModule[A]): ScalettaModule[A] =
    withMethodRegistrySettings(_.requireContexts(contexts: _*))(underlying)

  /**
   * Updates the default method registry settings for the underlying module
   */
  def withMethodRegistrySettings[A](fn: MethodRegistry.Settings => MethodRegistry.Settings)
                                   (underlying: ScalettaModule[A]): ScalettaModule[A] =
    ScalettaModule { registry =>
      registry.methodRegistry.pushSettings(fn)
      val result = underlying.configure(registry)
      registry.methodRegistry.popSettings()
      result
    }

  def composite(modules: ScalettaModule[_]*): ScalettaModule[Unit] =
    fromSeq(modules)

  def fromSeq(modules: Seq[ScalettaModule[_]]): ScalettaModule[Unit] =
    new Composite(modules)

  def sequence[A](modules: Seq[ScalettaModule[A]]): ScalettaModule[Seq[A]] =
    traverse(modules)(identity)

  def when(condition: Boolean)(module: => ScalettaModule[Unit]): ScalettaModule[Unit] =
    if (condition) module else ScalettaModule.empty

  def unless(condition: Boolean)(module: => ScalettaModule[Unit]): ScalettaModule[Unit] =
    if (!condition) module else ScalettaModule.empty

  def traverse[A, B](items: Seq[A])(fn: A => ScalettaModule[B]): ScalettaModule[Seq[B]] =
    new Traversed(items, fn)

  def combine[A, Result](ma: ScalettaModule[A])
                        (fn: A => Result): ScalettaModule[Result] =
    ma.map(fn)

  def combine[A, B, Result](ma: ScalettaModule[A],
                            mb: ScalettaModule[B])
                           (fn: (A, B) => Result): ScalettaModule[Result] =
    new Zipped2(ma, mb, fn)

  def combine[A, B, C, Result](ma: ScalettaModule[A],
                               mb: ScalettaModule[B],
                               mc: ScalettaModule[C])
                              (fn: (A, B, C) => Result): ScalettaModule[Result] =
    new Zipped3(ma, mb, mc, fn)

  def combine[A, B, C, D, Result](ma: ScalettaModule[A],
                                  mb: ScalettaModule[B],
                                  mc: ScalettaModule[C],
                                  md: ScalettaModule[D])
                                 (fn: (A, B, C, D) => Result): ScalettaModule[Result] =
    new Zipped4(ma, mb, mc, md, fn)

  def combine[A, B, C, D, E, Result](ma: ScalettaModule[A],
                                     mb: ScalettaModule[B],
                                     mc: ScalettaModule[C],
                                     md: ScalettaModule[D],
                                     me: ScalettaModule[E])
                                    (fn: (A, B, C, D, E) => Result): ScalettaModule[Result] =
    new Zipped5(ma, mb, mc, md, me, fn)

  def combine[A, B, C, D, E, F, Result](ma: ScalettaModule[A],
                                        mb: ScalettaModule[B],
                                        mc: ScalettaModule[C],
                                        md: ScalettaModule[D],
                                        me: ScalettaModule[E],
                                        mf: ScalettaModule[F])
                                       (fn: (A, B, C, D, E, F) => Result): ScalettaModule[Result] =
    new Zipped6(ma, mb, mc, md, me, mf, fn)

  def combine[A, B, C, D, E, F, G, Result](ma: ScalettaModule[A],
                                           mb: ScalettaModule[B],
                                           mc: ScalettaModule[C],
                                           md: ScalettaModule[D],
                                           me: ScalettaModule[E],
                                           mf: ScalettaModule[F],
                                           mg: ScalettaModule[G])
                                          (fn: (A, B, C, D, E, F, G) => Result): ScalettaModule[Result] =
    new Zipped7(ma, mb, mc, md, me, mf, mg, fn)

  def combine[A, B, C, D, E, F, G, H, Result](ma: ScalettaModule[A],
                                              mb: ScalettaModule[B],
                                              mc: ScalettaModule[C],
                                              md: ScalettaModule[D],
                                              me: ScalettaModule[E],
                                              mf: ScalettaModule[F],
                                              mg: ScalettaModule[G],
                                              mh: ScalettaModule[H])
                                             (fn: (A, B, C, D, E, F, G, H) => Result): ScalettaModule[Result] =
    new Zipped8(ma, mb, mc, md, me, mf, mg, mh, fn)

  def combine[A, B, C, D, E, F, G, H, I, Result](ma: ScalettaModule[A],
                                                 mb: ScalettaModule[B],
                                                 mc: ScalettaModule[C],
                                                 md: ScalettaModule[D],
                                                 me: ScalettaModule[E],
                                                 mf: ScalettaModule[F],
                                                 mg: ScalettaModule[G],
                                                 mh: ScalettaModule[H],
                                                 mi: ScalettaModule[I])
                                                (fn: (A, B, C, D, E, F, G, H, I) => Result): ScalettaModule[Result] =
    new Zipped9(ma, mb, mc, md, me, mf, mg, mh, mi, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, Result](ma: ScalettaModule[A],
                                                    mb: ScalettaModule[B],
                                                    mc: ScalettaModule[C],
                                                    md: ScalettaModule[D],
                                                    me: ScalettaModule[E],
                                                    mf: ScalettaModule[F],
                                                    mg: ScalettaModule[G],
                                                    mh: ScalettaModule[H],
                                                    mi: ScalettaModule[I],
                                                    mj: ScalettaModule[J])
                                                   (fn: (A, B, C, D, E, F, G, H, I, J) => Result): ScalettaModule[Result] =
    new Zipped10(ma, mb, mc, md, me, mf, mg, mh, mi, mj, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, K, Result](ma: ScalettaModule[A],
                                                       mb: ScalettaModule[B],
                                                       mc: ScalettaModule[C],
                                                       md: ScalettaModule[D],
                                                       me: ScalettaModule[E],
                                                       mf: ScalettaModule[F],
                                                       mg: ScalettaModule[G],
                                                       mh: ScalettaModule[H],
                                                       mi: ScalettaModule[I],
                                                       mj: ScalettaModule[J],
                                                       mk: ScalettaModule[K])
                                                      (fn: (A, B, C, D, E, F, G, H, I, J, K) => Result): ScalettaModule[Result] =
    new Zipped11(ma, mb, mc, md, me, mf, mg, mh, mi, mj, mk, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, K, L, Result](ma: ScalettaModule[A],
                                                          mb: ScalettaModule[B],
                                                          mc: ScalettaModule[C],
                                                          md: ScalettaModule[D],
                                                          me: ScalettaModule[E],
                                                          mf: ScalettaModule[F],
                                                          mg: ScalettaModule[G],
                                                          mh: ScalettaModule[H],
                                                          mi: ScalettaModule[I],
                                                          mj: ScalettaModule[J],
                                                          mk: ScalettaModule[K],
                                                          ml: ScalettaModule[L])
                                                         (fn: (A, B, C, D, E, F, G, H, I, J, K, L) => Result): ScalettaModule[Result] =
    new Zipped12(ma, mb, mc, md, me, mf, mg, mh, mi, mj, mk, ml, fn)

  def combine[A, B, C, D, E, F, G, H, I, J, K, L, M, Result](ma: ScalettaModule[A],
                                                             mb: ScalettaModule[B],
                                                             mc: ScalettaModule[C],
                                                             md: ScalettaModule[D],
                                                             me: ScalettaModule[E],
                                                             mf: ScalettaModule[F],
                                                             mg: ScalettaModule[G],
                                                             mh: ScalettaModule[H],
                                                             mi: ScalettaModule[I],
                                                             mj: ScalettaModule[J],
                                                             mk: ScalettaModule[K],
                                                             ml: ScalettaModule[L],
                                                             mm: ScalettaModule[M])
                                                            (fn: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Result): ScalettaModule[Result] =
    new Zipped13(ma, mb, mc, md, me, mf, mg, mh, mi, mj, mk, ml, mm, fn)

  private final class Zipped13[A, B, C, D, E, F, G, H, I, J, K, L, M, Result](ma: ScalettaModule[A],
                                                                              mb: ScalettaModule[B],
                                                                              mc: ScalettaModule[C],
                                                                              md: ScalettaModule[D],
                                                                              me: ScalettaModule[E],
                                                                              mf: ScalettaModule[F],
                                                                              mg: ScalettaModule[G],
                                                                              mh: ScalettaModule[H],
                                                                              mi: ScalettaModule[I],
                                                                              mj: ScalettaModule[J],
                                                                              mk: ScalettaModule[K],
                                                                              ml: ScalettaModule[L],
                                                                              mm: ScalettaModule[M],
                                                                              fn: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Result) extends ScalettaModule[Result] {
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

  private def mapped[A, B](underlying: ScalettaModule[A],
                           fn: A => B): ScalettaModule[B] =
    new Mapped[A, B](underlying, fn)

  private def flatMapped[A, B](underlying: ScalettaModule[A],
                               fn: A => ScalettaModule[B]): ScalettaModule[B] =
    new FlatMapped[A, B](underlying, fn)

  private def zip[A, B](ma: ScalettaModule[A], mb: ScalettaModule[B]): ScalettaModule[(A, B)] =
    new Zipped2(ma, mb, (a: A, b: B) => (a, b))

  private class Basic[A](fn: Setup => A) extends ScalettaModule[A] {
    def configure(setup: Setup): A = fn(setup)
  }


  private class Mapped[A, B](underlying: ScalettaModule[A],
                             fn: A => B) extends ScalettaModule[B] {
    def configure(setup: Setup): B =
      fn(underlying.configure(setup))
  }

  private class FlatMapped[A, B](underlying: ScalettaModule[A],
                                 fn: A => ScalettaModule[B]) extends ScalettaModule[B] {
    def configure(setup: Setup): B =
      fn(underlying.configure(setup)).configure(setup)
  }

  private class Composite(components: Seq[ScalettaModule[_]]) extends ScalettaModule[Unit] {
    def configure(setup: Setup): Unit = {
      components.foreach(_.configure(setup))
    }
  }

  private final class Traversed[A, B](items: Seq[A],
                                      fn: A => ScalettaModule[B]) extends ScalettaModule[Seq[B]] {
    def configure(setup: Setup): Seq[B] = {
      items.foldLeft(Vector.empty[B]) { (acc, item) =>
        acc :+ fn(item).configure(setup)
      }
    }
  }

  private final class Zipped2[A, B, Result](ma: ScalettaModule[A],
                                            mb: ScalettaModule[B],
                                            fn: (A, B) => Result) extends ScalettaModule[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      fn(a, b)
    }
  }

  private final class Zipped3[A, B, C, Result](ma: ScalettaModule[A],
                                               mb: ScalettaModule[B],
                                               mc: ScalettaModule[C],
                                               fn: (A, B, C) => Result) extends ScalettaModule[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      fn(a, b, c)
    }
  }

  private final class Zipped4[A, B, C, D, Result](ma: ScalettaModule[A],
                                                  mb: ScalettaModule[B],
                                                  mc: ScalettaModule[C],
                                                  md: ScalettaModule[D],
                                                  fn: (A, B, C, D) => Result) extends ScalettaModule[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      fn(a, b, c, d)
    }
  }

  private final class Zipped5[A, B, C, D, E, Result](ma: ScalettaModule[A],
                                                     mb: ScalettaModule[B],
                                                     mc: ScalettaModule[C],
                                                     md: ScalettaModule[D],
                                                     me: ScalettaModule[E],
                                                     fn: (A, B, C, D, E) => Result) extends ScalettaModule[Result] {
    def configure(setup: Setup): Result = {
      val a = ma.configure(setup)
      val b = mb.configure(setup)
      val c = mc.configure(setup)
      val d = md.configure(setup)
      val e = me.configure(setup)
      fn(a, b, c, d, e)
    }
  }

  private final class Zipped6[A, B, C, D, E, F, Result](ma: ScalettaModule[A],
                                                        mb: ScalettaModule[B],
                                                        mc: ScalettaModule[C],
                                                        md: ScalettaModule[D],
                                                        me: ScalettaModule[E],
                                                        mf: ScalettaModule[F],
                                                        fn: (A, B, C, D, E, F) => Result) extends ScalettaModule[Result] {
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

  private final class Zipped7[A, B, C, D, E, F, G, Result](ma: ScalettaModule[A],
                                                           mb: ScalettaModule[B],
                                                           mc: ScalettaModule[C],
                                                           md: ScalettaModule[D],
                                                           me: ScalettaModule[E],
                                                           mf: ScalettaModule[F],
                                                           mg: ScalettaModule[G],
                                                           fn: (A, B, C, D, E, F, G) => Result) extends ScalettaModule[Result] {
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

  private final class Zipped8[A, B, C, D, E, F, G, H, Result](ma: ScalettaModule[A],
                                                              mb: ScalettaModule[B],
                                                              mc: ScalettaModule[C],
                                                              md: ScalettaModule[D],
                                                              me: ScalettaModule[E],
                                                              mf: ScalettaModule[F],
                                                              mg: ScalettaModule[G],
                                                              mh: ScalettaModule[H],
                                                              fn: (A, B, C, D, E, F, G, H) => Result) extends ScalettaModule[Result] {
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

  private final class Zipped9[A, B, C, D, E, F, G, H, I, Result](ma: ScalettaModule[A],
                                                                 mb: ScalettaModule[B],
                                                                 mc: ScalettaModule[C],
                                                                 md: ScalettaModule[D],
                                                                 me: ScalettaModule[E],
                                                                 mf: ScalettaModule[F],
                                                                 mg: ScalettaModule[G],
                                                                 mh: ScalettaModule[H],
                                                                 mi: ScalettaModule[I],
                                                                 fn: (A, B, C, D, E, F, G, H, I) => Result) extends ScalettaModule[Result] {
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

  private final class Zipped10[A, B, C, D, E, F, G, H, I, J, Result](ma: ScalettaModule[A],
                                                                     mb: ScalettaModule[B],
                                                                     mc: ScalettaModule[C],
                                                                     md: ScalettaModule[D],
                                                                     me: ScalettaModule[E],
                                                                     mf: ScalettaModule[F],
                                                                     mg: ScalettaModule[G],
                                                                     mh: ScalettaModule[H],
                                                                     mi: ScalettaModule[I],
                                                                     mj: ScalettaModule[J],
                                                                     fn: (A, B, C, D, E, F, G, H, I, J) => Result) extends ScalettaModule[Result] {
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

  private final class Zipped11[A, B, C, D, E, F, G, H, I, J, K, Result](ma: ScalettaModule[A],
                                                                        mb: ScalettaModule[B],
                                                                        mc: ScalettaModule[C],
                                                                        md: ScalettaModule[D],
                                                                        me: ScalettaModule[E],
                                                                        mf: ScalettaModule[F],
                                                                        mg: ScalettaModule[G],
                                                                        mh: ScalettaModule[H],
                                                                        mi: ScalettaModule[I],
                                                                        mj: ScalettaModule[J],
                                                                        mk: ScalettaModule[K],
                                                                        fn: (A, B, C, D, E, F, G, H, I, J, K) => Result) extends ScalettaModule[Result] {
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

  private final class Zipped12[A, B, C, D, E, F, G, H, I, J, K, L, Result](ma: ScalettaModule[A],
                                                                           mb: ScalettaModule[B],
                                                                           mc: ScalettaModule[C],
                                                                           md: ScalettaModule[D],
                                                                           me: ScalettaModule[E],
                                                                           mf: ScalettaModule[F],
                                                                           mg: ScalettaModule[G],
                                                                           mh: ScalettaModule[H],
                                                                           mi: ScalettaModule[I],
                                                                           mj: ScalettaModule[J],
                                                                           mk: ScalettaModule[K],
                                                                           ml: ScalettaModule[L],
                                                                           fn: (A, B, C, D, E, F, G, H, I, J, K, L) => Result) extends ScalettaModule[Result] {
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
