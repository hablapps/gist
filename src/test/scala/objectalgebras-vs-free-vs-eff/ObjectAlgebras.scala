package org.hablapps.gist

import org.scalatest._
import cats.{Apply, Functor, Monad, MonadState, Eval}
import cats.data.State
import scala.util.Try

/*
This gist shows how we can use and combine algebras (in this case a custom
algebra IOAlg and Monad from cats library) to successfully build programs
that are interpretation free.

IMPORTANT: Compare this pattern with the one described on `FreeMonad.scala`
*/
class ObjectAlgebras extends FlatSpec with Matchers {

  // We start off with the definition of our custom algebra. This algebra
  // represents the effect of reading and writing from some IO interface.
  trait IOAlg[F[_]] {
    def read: F[String]
    def write(s: String): F[Unit]
  }

  // We add some syntax to ease the proccess of writing programs with this
  // algebra. These "smart constructors" are actually church representations
  // of the basic operations of our algebra.
  object IOAlg {
    object syntax {
      def read[F[_]](implicit F: IOAlg[F]): F[String] =
        F.read
      def write[F[_]](s: String)(implicit F: IOAlg[F]): F[Unit] =
        F.write(s)
    }
  }

  object InitialInstance {

    // OPTIONAL: This would be the initial instance of our algebra. It's
    // a church encoding representation of the initial algebra. This representation
    // is not needed for defining our simple programs as they are already
    // church representations on their own.
    // trait IO[A] {
    //   def apply[F[_]: IOAlg]: F[A]
    // }
  }

  // Now we'll write some simple programs to show how we can use algebras
  // to produce interpretation-free programs.
  object GenericPrograms {
    import IOAlg.syntax._, cats.syntax.flatMap._, cats.syntax.functor._, cats.syntax.cartesian._

    // In order to write this "echo" program, we need two algebras:
    // - IOAlg: to be able to write and read from somewhere
    // - Monad: to write sequential programs
    def echo[F[_]: IOAlg: Monad]: F[Unit] =
      read >>=
      write[F]

    def askName[F[_]: IOAlg: Monad]: F[String] =
      write("What's your name?") >>
      read

    // For the following scenario, we don't need the `Monad` algebra: it's 
    // too much powerful for our needs. With `Functor` we have enough power, as
    // it allows us to modify the content of a structure, right what we
    // need.
    def toInt[F[_]: IOAlg: Functor]: F[Option[Int]] =
      read map { s =>
        Try(s.toInt).toOption
      }

    // In this case we need an intermediate point between `Functor` and
    // `Monad` algebras. `Apply` algebra empowers us to do computations
    // in parallel and aggregate the results.
    def doubleRead[F[_]: IOAlg: Apply]: F[String] =
      (read |@| read).map(_ + _)

  }

  // Now we write an effectful instance of our algebra, it works with
  // the standard input/output of our system.
  object EvalInstance {
    import GenericPrograms._

    implicit object IOEval extends IOAlg[Eval] {
      def read: Eval[String] = 
        Eval.always(scala.io.StdIn.readLine)

      def write(s: String): Eval[Unit] = 
        Eval.always(println(s))
    }

    // It doesn't make much sense to write tests for an impure interpreter
    // but just to show how it works, here they are.
    object Test {
      print("Write anything: ")
      echo[Eval].value shouldBe (())

      print("Write `John Doe`: ")
      askName[Eval].value shouldBe "John Doe"

      print("Write `123`: ")
      toInt[Eval].value shouldBe Option(123)
      print("Write anything that's not a number: ")
      toInt[Eval].value shouldBe Option.empty

      println("Write `hello` and then `world`:")
      doubleRead[Eval].value shouldBe "helloworld"
    }
  }

  // And for testing purposes we give a pure instance that works with
  // the `MonadState` algebra.
  object StateInstance {
    import GenericPrograms._

    case class IOState(readList: List[String], writeList: List[String]) {
      def read: IOState = this.copy(readList = readList.tail)
      def write(s: String): IOState = this.copy(writeList = s :: writeList)
    }

    type IOAction[A] = State[IOState, A]

    implicit object stateIO extends IOAlg[IOAction] {
      val monadState = implicitly[MonadState[IOAction, IOState]]
      import monadState._
  
      def read: IOAction[String] =
        for {
          s <- get
          _ <- set(s.read)
        } yield s.readList.head

      def write(msg: String): IOAction[Unit] =
        modify(_.write(msg))
    }

    // Usually we want to write tests for a pure interpretation like this one
    // We can verify every input, output and result.
    object Test {
      echo[IOAction].run(IOState("hello!" :: Nil, Nil)).value shouldBe
        (IOState(Nil, "hello!" :: Nil), ())

      askName[IOAction].run(IOState("Javier Fuentes" :: Nil, Nil)).value shouldBe
        (IOState(Nil, "What's your name?" :: Nil), "Javier Fuentes")

      toInt[IOAction].run(IOState("123" :: Nil, Nil)).value shouldBe
        (IOState(Nil, Nil), Option(123))
      
      toInt[IOAction].run(IOState("This is not an Integer" :: Nil, Nil)).value shouldBe
        (IOState(Nil, Nil), Option.empty)

      doubleRead[IOAction].run(IOState("Reading 1" :: "Reading 2" :: Nil, Nil)).value shouldBe
        (IOState(Nil, Nil), "Reading 1Reading 2")
    }
  }

  // Uncomment this if you want to try the effectful interpreter
  // "IO programs with Eval" should "work fine" in {
  //   EvalInstance.Test
  // }

  "IO programs with State" should "work fine" in {
    StateInstance.Test
  }

}

object ObjectAlgebras extends ObjectAlgebras