package org.hablapps.gist
package coalgebras
package scalazimpl

object CofreezFinalCoalgebra{
  import scalaz.{Monad, State, StateT, ~>}, scalaz.Cofree._
  
  import Automata._, Input._

  type CofreeF[F[_],I,S] = ADT[I,?] ~> λ[T=>F[(S, T)]]
  type Cofree[F[_],I,Y] = scalaz.Cofree[CofreeF[F,I,?],Y]

  def cofree[F[_]: Monad, I] = 
    new CofreeCoalgebra[Cofree[F,I,?], Automata[F,I,?]]{ self => 
      
      def machine[Y] = new Automata[F, I, Cofree[F,I,Y]]{
        def isFinal() = StateT{ _.tail(IsFinal()) }
        def next(i: I) = StateT{ _.tail(Next(i)) }
      }

      def label[X](cf: Cofree[F,I,X]): X = 
        cf.extract

      import scalaz.std.tuple._, scalaz.syntax.functor._, scalaz.syntax.bitraverse._

      def trace[X,Y](f: X => Y)(implicit 
        coalg: Automata[F,I,X]): X => Cofree[F,I,Y] = { x: X => 
        scalaz.Cofree[CofreeF[F,I,?],Y](f(x), new CofreeF[F,I,Cofree[F,I,Y]]{
          def apply[O](input: ADT[I,O]) = 
            coalg(input).apply(x)
              .map(_.bimap(trace(f)(coalg),identity))
        })
      }
    }    
}

import org.scalatest.{FlatSpec, Matchers}

class CofreezFinalCoalgebra extends FlatSpec with Matchers{
  import CofreeCoalgebra._, Syntax._
  import CofreezFinalCoalgebra._
  import Automata.Syntax._, AnAutomata._
  import scalaz.Id, Id._

  "Cofree comonad cofree coalgebra" should "simulate `Even`" in {

    implicit val CofreeAutomata = cofree[Id,Boolean]
    implicit val CofreeAutomataMachine = CofreeAutomata.machine[String]

    // Language accepted from state 0

    val initial0: Cofree[Id,Boolean,String] = 
      CofreeAutomata.trace[Int,String](_.toString)(Even[Id])(0)

    // Behaviours are machines!

    initial0.isFinal() shouldBe true
    initial0.label() shouldBe "0"
    
    initial0.next(true).isFinal() shouldBe false
    (initial0.next(true): Cofree[Id,Boolean,String]).label() shouldBe "1"
  }

}
