package org.hablapps.gist

import org.scalatest._

import scalaz._, Scalaz._

object VanLaarhoven extends FlatSpec with Matchers {

  // Iso

  trait Iso[S, A] {
    def apply[F[_]: Functor, G[_]: Functor](f: G[A] => F[A])(s: G[S]): F[S]
  }

  object Iso {

    implicit def asMonocleIso[S, A](iso: Iso[S, A]): monocle.Iso[S, A] =
      monocle.Iso[S, A](
        s => iso[Const[A, ?], Id](Const.apply)(s).getConst)(
        a => iso[Id, Const[A, ?]](_.getConst)(Const(a)))
  }

  // Prism

  trait Profunctor[F[_, _]] {
    def dimap[A, B, C, D](f: B => A)(g: C => D): F[A, C] => F[B, D]
  }

  // State is not a profunctor, since we require an additional `A => B` to
  // restore the state. So it's invariant in that position.
  //
  // object StateProfunctor extends Profunctor[State] {
  //   def dimap[A, B, C, D](f: B => A)(g: C => D): State[A, C] => State[B, D] =
  //     sac => State[B, D](b => sac.run(f(b)))
  // }

  trait Choice[P[_, _]] extends Profunctor[P] {
    def left[A, B, C](p: P[A, B]): P[A \/ C, B \/ C]
    def right[A, B, C](p: P[A, B]): P[C \/ A, C \/ B]
  }

  implicit object FunctionChoice extends Choice[Function1] {

    def dimap[A, B, C, D](f: B => A)(g: C => D): (A => C) => B => D =
      fac => g compose fac compose f

    def left[A, B, C](f: A => B): A \/ C => B \/ C =
      _.fold(f(_).left, _.right)

    def right[A, B, C](f: A => B): C \/ A => C \/ B =
      _.fold(_.left, f(_).right)
  }

  // What is this: Van Laarhoven, Pure Profunctor?
  trait Prism[S, A] {
    def apply[P[_, _]: Choice, F[_]: Applicative](p: P[A, F[A]]): P[S, F[S]]
  }

  // TODO: don't know how to deal with Van Laarhoven Prisms at all!!!
  object Prism {

    def asMonoclePrism[S, A](prism: Prism[S, A]): monocle.Prism[S, A] =
      monocle.Prism[S, A](
        s => ???)(
        a => ???)
  }

  // Lens

  trait Lens[S, A] {
    def apply[F[_]: Functor](f: A => F[A])(s: S): F[S]
  }

  object Lens {

    def asMonocleLens[S, A](lens: Lens[S, A]): monocle.Lens[S, A] =
      monocle.Lens[S, A](
        s => lens.apply[Const[A, ?]](Const.apply)(s).getConst)(
        a => lens.apply[Id](_ => a))
  }

  // Polymorphic Lens

  trait PLens[S, T, A, B] {
    def apply[F[_]: Functor](f: A => F[B])(s: S): F[T]
  }

  object PLens {

    def asMonoclePLens[S, T, A, B](
        plens: PLens[S, T, A, B]): monocle.PLens[S, T, A, B] =
      monocle.PLens[S, T, A, B](
        s => plens.apply[Const[A, ?]](Const.apply)(s).getConst)(
        b => plens.apply[Id](_ => b))
  }

  // Optional

  // TODO: is this just a traversal optimisation?
  trait Optional[S, A] {
    def apply[F[_]: Applicative](f: A => F[A])(s: S): F[S]
  }

  object Optional {

    def asMonocleOptional[S, A](opt: Optional[S, A]): monocle.Optional[S, A] =
      monocle.Optional[S, A](
        s => ???)(
        a => opt[Id](_ => a))
  }

  // Getter

  trait Getter[S, A] {
    def apply[F[_]: Functor: Contravariant](f: A => F[A])(s: S): F[S]
  }

  object Getter {

    def asMonocleGetter[S, A](getter: Getter[S, A]): monocle.Getter[S, A] =
      monocle.Getter[S, A](s => getter[Const[A, ?]](Const.apply)(s).getConst)
  }

  // Setter

  trait Setter[S, A] {
    def apply(f: A => Id[A]): S => Id[S]
  }

  object Setter {

    def asMonocleSetter[S, A](setter: Setter[S, A]): monocle.Setter[S, A] =
      monocle.Setter[S, A](setter.apply)
  }

  // Fold

  trait Fold[S, A] {
    def apply[F[_]: Contravariant: Applicative](f: A => F[A])(s: S): F[S]
  }

  object Fold {

    def asMonocleFold[S, A](fld: Fold[S, A]): monocle.Fold[S, A] =
      new monocle.Fold[S, A] {
        def foldMap[M: Monoid](f: A => M)(s: S): M =
          fld[Const[M, ?]](f andThen Const.apply)(s).getConst
      }
  }

  // Traversal

  trait Traversal[S, A] {
    def apply[F[_]: Applicative](f: A => F[A])(s: S): F[S]
  }
}
