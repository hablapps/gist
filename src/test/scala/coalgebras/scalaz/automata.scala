package org.hablapps.gist
package coalgebras
package scalazimpl

import scalaz.{StateT, State}

/* Coalgebra specification */

trait Automata[F[_],I,S] extends Automata.Input[I,StateT[F,S,?]]{
  type Program[T]=StateT[F,S,T]
}

object Automata{
 
  /* Input language of the machine */
  
  trait Input[I,P[_]]{
    def isFinal(): P[Boolean]
    def next(i: I): P[Unit]
  }

  object Input{
  
    object Syntax{
      def isFinal[I,P[_]]()(implicit I: Input[I,P]) = I.isFinal()
      def next[I,P[_]](i: I)(implicit I: Input[I,P]) = I.next(i)
    }

    abstract sealed class ADT[I,_]
    case class IsFinal[I]() extends ADT[I,Boolean]
    case class Next[I](i: I) extends ADT[I,Unit]

    type InputF[I,P[_],T]=ADT[I,T]

    type IOAutomata[F[_],I,S]=
      IO.CoalgebraFromFAlgebra[InputF[I,?[_],?],StateT[F,?,?],S]
  }

  /* Automata are entities */
  
  implicit def toEntity[F[_],I,S](automata: Automata[F,I,S]): Entity[F,Input.ADT[I,?],S] =
    new Entity[F,Input.ADT[I,?],S]{
      def apply[X](input: Input.ADT[I,X]): StateT[F,S,X] = input match{
        case Input.IsFinal() => automata.isFinal
        case Input.Next(i) => automata.next(i)
      }
    }

  /* Auxiliary helpers */
  
  import scalaz.Monad

  def apply[F[_]: Monad,I,S](_isFinal: S => Boolean, _next: I => S => S): Automata[F,I,S] = 
    new Automata[F,I,S]{
      val M = StateT.stateTMonadState[S,F]
      def isFinal(): Program[Boolean] = M.gets(_isFinal)
      def next(i: I): Program[Unit] = M.modify(_next(i))
    }

  object Syntax{
    implicit class AutomataOps[F[_]: Monad,I,S](s: S)(implicit A: Automata[F,I,S]){
      def isFinal(): F[Boolean] = A.isFinal().eval(s)
      def next(i: I): F[S] = A.next(i).exec(s)
    }
  }
}
