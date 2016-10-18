package org.hablapps.gist

import org.scalatest._

import scalaz._, Scalaz._

object IOCoalgebra extends FlatSpec with Matchers {

  type IOCoalgebra[IOAlg[_[_]], Step[_, _], S] = IOAlg[Step[S, ?]]

  // Cool as a Pope name
  type PIOCoalgebra[IOAlg[_[_]], Step[_, _, _], S, T] = IOAlg[Step[S, T, ?]]

  // Iso

  // Prism

  // Lens

  trait LensAlg[A, P[_]] {

    def get: P[A]

    def set(a: A): P[Unit]
  }

  type Lens[S, A] = IOCoalgebra[LensAlg[A, ?[_]], State, S]

  object Lens {

    def asMonocleLens[S, A](lens: Lens[S, A]): monocle.Lens[S, A] =
      monocle.Lens[S, A](lens.get.eval)(a => lens.set(a).exec)
  }

  // Polymorphic PLens

  trait PLensAlg[A, B, P[_]] {

    def get: P[A]

    def set(a: B): P[Unit]
  }

  type PLens[S, T, A, B] = PIOCoalgebra[PLensAlg[A, B, ?[_]], IndexedState, S, T]

  object PLens {

    def asMonoclePLens[S, T, A, B](
        plens: PLens[S, T, A, B]): monocle.PLens[S, T, A, B] =
      monocle.PLens[S, T, A, B](plens.get.eval)(a => plens.set(a).exec)
  }

  // Optional

  trait OptionalAlg[A, P[_]] {

    def getOption: P[Option[A]]

    def set(a: A): P[Unit]
  }

  type Optional[S, A] = IOCoalgebra[OptionalAlg[A, ?[_]], State, S]

  object Optional {

    def asMonocleOptional[S, A](opt: Optional[S, A]): monocle.Optional[S, A] =
      monocle.Optional[S, A](opt.getOption.eval)(a => opt.set(a).exec)
  }

  // Getter

  trait GetterAlg[A, P[_]] {
    def get: P[A]
  }

  type Getter[S, A] = IOCoalgebra[GetterAlg[A, ?[_]], Function1, S]

  object Getter {

    def asMonocleGetter[S, A](getter: Getter[S, A]): monocle.Getter[S, A] =
      monocle.Getter[S, A](getter.get)
  }

  // Setter

  trait SetterAlg[A, P[_]] {
    def modify(f: A => A): P[Unit]
  }

  type Setter[S, A] = IOCoalgebra[SetterAlg[A, ?[_]], State, S]

  object Setter {

    def asMonocleSetter[S, A](setter: Setter[S, A]): monocle.Setter[S, A] =
      monocle.Setter[S, A](f => setter.modify(f).exec)
  }

  // Fold

  trait FoldAlg[A, P[_]] {
    def foldMap[M: Monoid](f: A => M): P[M]
  }

  type Fold[S, A] = IOCoalgebra[FoldAlg[A, ?[_]], Function1, S]

  object Fold {

    def asMonocleFold[S, A](fld: Fold[S, A]): monocle.Fold[S, A] =
      new monocle.Fold[S, A] {
        def foldMap[M: Monoid](f: A => M)(s: S): M = fld.foldMap(f).apply(s)
      }
  }

  // Traversal
}
