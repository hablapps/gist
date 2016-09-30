package org.hablapps.gist
package coalgebras
package scalazimpl

import org.scalatest.{FlatSpec, Matchers}

/* Programming an automata applicatively */

object ImperativeProgramming{

  import scalaz.Monad, scalaz.syntax.monad._
  import Automata.Input.Syntax._

  def largestPrefix[P[_]: Automata.Input[Boolean,?[_]]: Monad](
    boolList: List[Boolean]): P[List[Boolean]] = boolList match {
      case Nil => List[Boolean]().point
      case head::tail => 
        next(head) >> isFinal.ifM(
          ifTrue = largestPrefix(tail) map ( head :: _ ),
          ifFalse = List[Boolean]().point
        )
    }

  def split[P[_]: Automata.Input[Boolean,?[_]]: Monad](
    boolList: List[Boolean]): P[(List[Boolean],List[Boolean])] = 
    boolList match {
      case Nil => (List[Boolean](),List[Boolean]()).point[P]
      case head::tail => 
        (next(head) >> isFinal).ifM(
          ifTrue = split(tail) map {
            case (list1,list2) => (head::list1,list2)
          },
          ifFalse = (List[Boolean](),boolList).point
        )
    }

}

class ImperativeProgramming extends FlatSpec with Matchers{
  import ImperativeProgramming._

  "Imperative programs" should "work" in {
    import AnAutomata._
    import scalaz.Id, Id._

    val EvenId = Even[Id]

    split[EvenId.Program](List(true,true,false)).eval(0) shouldBe (List(), List(true,true,false))
    split[EvenId.Program](List(false,false,true)).eval(0) shouldBe (List(false,false), List(true))
    split[EvenId.Program](List(false,false,false)).eval(0) shouldBe (List(false,false,false), List())

    largestPrefix[EvenId.Program](List(true,false,true)).eval(0) shouldBe List()
    largestPrefix[EvenId.Program](List(false,false,true)).eval(0) shouldBe List(false,false)
    largestPrefix[EvenId.Program](List(false,false,false)).eval(0) shouldBe List(false,false,false)

  }
}