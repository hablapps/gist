package org.hablapps.gist

import org.scalatest._

object Adhoc extends FlatSpec with Matchers {

  // Iso

  trait Iso[S, A] {

    def get(s: S): A

    def reverseGet(a: A): S
  }

  object Iso {

    def asMonocleIso[S, A](iso: Iso[S, A]): monocle.Iso[S, A] =
      monocle.Iso[S, A](iso.get)(iso.reverseGet)
  }

  // Prism

  trait Prism[S, A] {

    def getOption(s: S): Option[A]

    def reverseGet(a: A): S
  }

  object Prism {

    def asMonoclePrism[S, A](prism: Prism[S, A]): monocle.Prism[S, A] =
      monocle.Prism[S, A](prism.getOption)(prism.reverseGet)
  }

  // Lens

  trait Lens[S, A] {

    def get(s: S): A

    def set(a: A): S => S
  }

  object Lens {

    def asMonocleLens[S, A](lens: Lens[S, A]): monocle.Lens[S, A] =
      monocle.Lens[S, A](lens.get)(lens.set)
  }

  // Polymorphic Lens

  trait PLens[S, T, A, B] {

    def get(s: S): A

    def set(b: B): S => T
  }

  object PLens {

    def asMonoclePLens[S, T, A, B](
        plens: PLens[S, T, A, B]): monocle.PLens[S, T, A, B] =
      monocle.PLens[S, T, A, B](plens.get)(plens.set)
  }

  // Optional

  trait Optional[S, A] {

    def getOption(s: S): Option[A]

    def set(a: A): S => S
  }

  object Optional {

    def asMonocleOptional[S, A](opt: Optional[S, A]): monocle.Optional[S, A] =
      monocle.Optional[S, A](opt.getOption)(opt.set)
  }

  // Getter

  trait Getter[S, A] {
    def get(s: S): A
  }

  object Getter {

    def asMonocleGetter[S, A](getter: Getter[S, A]): monocle.Getter[S, A] =
      monocle.Getter[S, A](getter.get)
  }

  // Setter

  trait Setter[S, A] {
    def modify(f: A => A): S => S
  }

  object Setter {

    def asMonocleSetter[S, A](setter: Setter[S, A]): monocle.Setter[S, A] =
      monocle.Setter[S, A](setter.modify)
  }

  // Fold

  import scalaz.Monoid

  trait Fold[S, A] {
    def foldMap[M: Monoid](f: A => M)(s: S): M
  }

  object Fold {

    def asMonocleFold[S, A](fld: Fold[S, A]): monocle.Fold[S, A] =
      new monocle.Fold[S, A] {
        def foldMap[M: Monoid](f: A => M)(s: S): M = fld.foldMap(f)(s)
      }
  }

  // Traversal
}
