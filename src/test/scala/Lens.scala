package org.hablapps.gist

import scalaz._, Scalaz._

object Lens{

  case class Lens[S,A](
    get: S => A,
    put: S => A => S
  )

  object Lens{
    def getset[S,A](s: S)(lens: Lens[S,A]): Boolean =
      lens.set(lens.get(s),s) == s
  }

  type NatLens[S,A] = State[A,?] ~> State[S,?]

  object NatLens{
    def from[S,A](nat: NatLens[S,A]): Lens[S,A] =
      Lens(nat(State.get[A]).eval,
        // a => nat(State.put(a)).exec)
        s => a => nat(State.put(a)).exec(s))

    implicit class TupleOps[A,B](t: (A,B)){
      def map1[C](f: A => C): (C,B) =
        (f(t._1),t._2)
    }

    def to[S,A](lens: Lens[S,A]): NatLens[S,A] =
      λ[State[A,?] ~> State[S,?]]{
        st => State(s => st(lens.get(s)).map1(lens.put(s)))
      }
  }

}