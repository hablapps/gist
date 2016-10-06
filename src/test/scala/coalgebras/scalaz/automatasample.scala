package org.hablapps.gist
package coalgebras
package scalazimpl

import org.scalatest.{FlatSpec, Matchers}

object AnAutomata{
  
  /* A particular automata */

  import scalaz.Monad

  implicit def Even[F[_]: Monad]: Automata[F,Boolean,Int] = 
    Automata[F, Boolean, Int](
      _ % 2 == 0,
      i => _ + (if (i) 1 else 0))
}

class AnAutomata extends FlatSpec with Matchers{
  import AnAutomata._

  "`Even` automata" should "work with `Id`" in {
    import scalaz.Id, Id._

    Even[Id].isFinal().eval(1) shouldBe false
    Even[Id].isFinal().eval(2) shouldBe true

    Even[Id].next(true).exec(1) shouldBe 2
    Even[Id].next(false).exec(1) shouldBe 1

    // Using syntactic helpers

    import Automata.Syntax._

    1.isFinal() shouldBe false
    2.isFinal() shouldBe true

    1.next(true) shouldBe 2
    1.next(false) shouldBe 1
  }

  "`Even` automata" should """work with `String \/ ?`""" in {
    import scalaz.\/, \/._
    type Errorful[T]=String\/T

    Even[Errorful].isFinal().eval(1) shouldBe right(false)
    Even[Errorful].isFinal().eval(2) shouldBe right(true)

    Even[Errorful].next(true).exec(1) shouldBe right(2)
    Even[Errorful].next(false).exec(1) shouldBe right(1)

    // Using syntactic helpers
    
    import Automata.Syntax._, scalaz.syntax.either._

    1.isFinal() shouldBe false.right
    2.isFinal() shouldBe true.right

    1.next(true) shouldBe 2.right
    1.next(false) shouldBe 1.right
  }
}
