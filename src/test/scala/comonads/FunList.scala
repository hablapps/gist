package org.hablapps.gist

sealed abstract class FunList[A, B, T]
case class Done[A, B, T](t: T) extends FunList[A, B, T]
case class More[A, B, T](a: A, f: FunList[A, B, B => T]) extends FunList[A, B, T]

object FunList{
  import scalaz.Comonad

  def funListComonad[A] = new Comonad[FunList[A, A, ?]]{
    def map[S, T](store: FunList[A, A, S])(f: S => T): FunList[A, A, T] =
      store match {
        case Done(s) => Done(f(s))
        case More(a, fl) => More(a, map(fl)(_ andThen f))
      }

    def duplicate[S](store: FunList[A, A, S]): FunList[A, A, FunList[A, A, S]] =
      ???

    def cobind[S, T](store: FunList[A, A, S])(f: FunList[A, A, S] => T): FunList[A, A, T] =
      ???
      // store match {
      //   case d@Done(s) =>
      //     Done(f(d))
      //   case More(a, fl: FunList[A, A, A => S]) =>
      //     More(a, cobind(fl)())
      // }

    def copoint[S](store: FunList[A, A, S]): S =
      store match {
        case Done(s) => s
        case More(a, fl) => copoint(fl)(a)
      }
  }
}
