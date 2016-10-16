package org.hablapps.gist

import org.scalatest._

import scalaz._, Scalaz._

object VanLaarhoven extends FlatSpec with Matchers {

  // Iso

  trait Iso[S, A] {
    def apply[F[_]: Functor, G[_]: Functor](f: G[A] => F[A])(s: G[S]): F[S]
  }

  object Iso {

    def toAdhocIso[S, A](iso: Iso[S, A]): Adhoc.Iso[S, A] = new Adhoc.Iso[S, A] {

      def get(s: S): A = iso[Const[A, ?], Id](Const.apply)(s).getConst

      def reverseGet(a: A): S = iso()()
    }
  }

  // Prism

  // Lens

  trait Lens[S, A] {
    def apply[F[_]: Functor](f: A => F[A])(s: S): F[S]
  }

  // Optional
}
