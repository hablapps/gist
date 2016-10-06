package org.hablapps.gist
package coalgebras
package scalazimpl

import org.scalatest.{FlatSpec, Matchers}

/* Programming an automata with the possibility of errors */
object ProgrammingWithExceptions{

  import scalaz.Monad, scalaz.syntax.monad._, Filter._
  import Automata.Input.Syntax._

  def test1[I, P[_]: Automata.Input[I,?[_]]: Monad: Filter](
    i1: I, i2: I, i3: I): P[Unit] =
    for {
      true  <- isFinal[I,P]
      _     <- next(i1)
      false <- isFinal
      _     <- next(i2)
      true  <- isFinal
      _     <- next(i3)
    } yield ()

  import ImperativeProgramming._

  def isPrefix[I](l1: List[I], l2: List[I]): Boolean = (l1,l2) match {
    case (Nil,_) => true
    case (head1::tail1, head2::tail2) if head1 == head2 => isPrefix(tail1,tail2)
    case _ => false
  }

  def test2[P[_]: Automata.Input[Boolean,?[_]]: Monad: Filter](
    l: List[Boolean]): P[Unit] = 
    for {
      l1 <- largestPrefix(l)
      true <- isPrefix(l1,l).point
      l2 <- largestPrefix(l1)
      true <- isPrefix(l2,l1).point
      l3 <- largestPrefix(l2)
      true <- isPrefix(l3,l2).point
    } yield ()
}

class ProgrammingWithExceptions extends FlatSpec with Matchers{
  import ProgrammingWithExceptions._

  "Programs with filter" should "work" in {
    import AnAutomata._ 
    import scalaz.\/, \/._
    import Filter._

    val EvenEither = Even[Location \/ ?]

    test1[Boolean, EvenEither.Program](true,true,true).eval(0) shouldBe right(())
    test2[EvenEither.Program](List(true,true,true)).eval(0) shouldBe right(())
  }

}