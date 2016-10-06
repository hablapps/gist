package org.hablapps.gist
package coalgebras
package scalazimpl

object CoChurchFinalCoalgebra{
  import scalaz.{Monad, State, StateT, ~>}
  import scalaz.Id, Id._
  import scalaz.syntax.monad._
  
  import Automata._, Input._

  trait Cofree[F[_],I,Y]{
    type X
    val x: X
    val f: X => Y
    val coalg: Automata[F,I,X]
  }

  def cofree[F[_]: Monad, I] = 
    new CofreeCoalgebra[Cofree[F,I,?], Automata[F,I,?]]{ self => 

      def machine[Y] = new Automata[F, I, Cofree[F,I,Y]]{
        
        def isFinal() = StateT{ cofree => 
          (cofree.coalg.isFinal().eval(cofree.x).map((cofree,_)))
        }
        
        def next(i: I) = StateT{ cofree => 
          cofree.coalg.next(i).exec(cofree.x).map{
              case x2 => (new Cofree[F,I,Y]{
                type X = cofree.X
                val x = x2
                val f = cofree.f
                val coalg = cofree.coalg
              },())
            } 
        }
      }

      def label[X](cf: Cofree[F,I,X]): X = 
        cf.f(cf.x)

      def trace[_X,Y](_f: _X => Y)(implicit 
        _coalg: Automata[F,I,_X]): _X => Cofree[F,I,Y] = { _x: _X => 
        new Cofree[F,I,Y]{
          type X = _X
          val x = _x
          val f = _f
          val coalg = _coalg
        }
      }
    }    
}

import org.scalatest.{FlatSpec, Matchers}

class CoChurchFinalCoalgebra extends FlatSpec with Matchers{
  import CofreeCoalgebra._, Syntax._
  import CoChurchFinalCoalgebra._
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
