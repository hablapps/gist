package org.hablapps.gist
package coalgebras
package scalazimpl

import org.scalatest.{FlatSpec, Matchers}

/* Programming an automata applicatively */

object ApplicativeProgramming{
  import scalaz.Apply, scalaz.syntax.apply._
  import Automata.Input.Syntax._

  def isTTFAccepted[P[_]: Automata.Input[Boolean,?[_]]: Apply]: P[Boolean] = 
    next(true) *>
    next(true) *> 
    next(false) *>
    isFinal

  def isAccepted[P[_]: Automata.Input[Boolean,?[_]]: Apply](
    boolList: List[Boolean]): P[Boolean] = boolList match {
      case Nil => isFinal[Boolean,P]
      case head::tail => next(head) *> isAccepted(tail)
    }
  
}

class ApplicativeProgramming extends FlatSpec with Matchers{
  import ApplicativeProgramming._

  "Applicative programs" should "work" in {
    import AnAutomata._
    import scalaz.Id, Id._

    val EvenId = Even[Id]

    isTTFAccepted[EvenId.Program].eval(0) shouldBe true

    isAccepted(List()).eval(0) shouldBe true
    isAccepted(List(true)).eval(0) shouldBe false
    isAccepted(List(false)).eval(0) shouldBe true
    isAccepted(List(false,true)).eval(0) shouldBe false
    isAccepted(List(false,false)).eval(0) shouldBe true
  }

}

