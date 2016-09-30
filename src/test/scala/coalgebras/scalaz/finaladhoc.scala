package org.hablapps.gist
package coalgebras
package scalazimpl

object AdHocFinalCoalgebra{

  import scalaz.Id, Id.Id

  type Language[I] = List[I]=>Boolean

  object Language{

    implicit def apply[I]: FinalCoalgebra[Language[I], Automata[Id,I,?]] =
      new FinalCoalgebra[Language[I], Automata[Id,I,?]]{

        def coalgebra: Automata[Id,I,Language[I]] =
          Automata(_(Nil),
            input => language => word => language(input::word))

        def unfold[X](coalg: Automata[Id,I,X]): X => Language[I] =
          x => {
            case Nil => coalg.isFinal().eval(x)
            case input::tail =>
              unfold(coalg)(coalg.next(input).exec(x))(tail)
          }
      }
  }
}

import org.scalatest.{FlatSpec, Matchers}

class AdHocFinalCoalgebra extends FlatSpec with Matchers{
  import AdHocFinalCoalgebra._, Automata.Syntax._, FinalCoalgebra.Syntax._

  "Ad-hoc final coalgebra" should "simulate `Even`" in {
    import AnAutomata._
    import scalaz.Id, Id._

    implicit val BoolLanguage = Language[Boolean]
    implicit val BoolLanguageCoalg = BoolLanguage.coalgebra

    // Language accepted from state 0

    val initial0: Language[Boolean] = BoolLanguage.unfold(Even[Id])(0)

    initial0(List()) shouldBe true
    initial0(List(true,false,true)) shouldBe true

    // Language accepted from state 1

    val initial1: Language[Boolean] = BoolLanguage.unfold(Even[Id])(1)

    initial1(List()) shouldBe false
    initial1(List(true,false,true)) shouldBe false
    initial1(List(true)) shouldBe true

    // Behaviours are machines!

    initial0.isFinal() shouldBe true
    initial1.isFinal() shouldBe false
    
    initial0.next(true).isFinal() shouldBe false
    initial1.next(true).isFinal() shouldBe true    
  }


}
