package org.hablapps.gist

import org.scalatest._
import cats._
import cats.implicits._
import cats.data.State
import cats.free.Free
import scala.util.Try

/*
This gist shows the use of Free (monad) to successfully build programs
that are interpretation free.

IMPORTANT: Compare this pattern with the one described on `ObjectAlgebras.scala`
*/
class FreeMonad extends FlatSpec with Matchers {

  // We start off with the definition of our `Functor`. This represents
  // the instructions of our language.
  sealed abstract class IOF[_]
  case object Read extends IOF[String]
  case class Write(msg: String) extends IOF[Unit]

  // OPTIONAL: This would be the equivalent of the IOAlg defined in
  // `ObjectAlgebras.scala#L19` although is not needed using this technique.
  // type IOAlg[F[_]] = IOF ~> F

  // Free[IOF, A] is just an initial instance of the algebra formed by IOF[_]
  object InitialInstance {
    type IO[A] = Free[IOF, A]

    // We add some syntax to ease the proccess of writing programs with this
    // algebra. These are usually called "smart constructors".
    object IO {
      object syntax {
        def read: IO[String] =
          Free.liftF(Read)

        def write(msg: String): IO[Unit] =
          Free.liftF(Write(msg))
      }

      // OPTIONAL: We could easily give an instance of `IOAlg[IO]`
      // object IOIOAlg extends IOAlg[IO] {
      //   def apply[A](fa: IOF[A]): IO[A] = fa match {
      //     case Read => syntax.read
      //     case Write(msg) => syntax.write(msg)
      //   }
      // }
    }
  }
  import InitialInstance.IO

  // Now we'll write some simple programs to show how we can use algebras
  // to produce interpretation-free programs.
  object GenericPrograms {
    import IO.syntax._
    import IO._

    // We just need to use the "smart constructors" defined above
    val echo: IO[Unit] =
      read >>=
      write

    val askName: IO[String] =
      write("What's your name?") >>
      read

    // Here we are still using the power of `Monad`, even when it's not
    // necessary at all. We only require it to be a `Functor`.
    val toInt: IO[Option[Int]] =
      read map { s =>
        Try(s.toInt).toOption
      }

    // Something similar occurs here, we only needed an `Apply`.
    val doubleRead: IO[String] =
      (read |@| read).map(_ + _)

  }

  // Now we write an effectful instance of our algebra, it works with
  // the standard input/output of our system.
  object EvalInstance {
    import GenericPrograms._

    object IOEval extends ~>[IOF, Eval] {
      def apply[A](fa: IOF[A]): Eval[A] = fa match {
        case Read => Eval.always(scala.io.StdIn.readLine)
        case Write(s) => Eval.always(println(s))
      }
    }

    // It doesn't make much sense to write test for an impure interpreter
    // but just to show how it works.
    object Test {
      print("Write anything: ")
      echo.foldMap(IOEval).value shouldBe (())

      print("Write `John Doe`: ")
      askName.foldMap(IOEval).value shouldBe "John Doe"

      print("Write `123`: ")
      toInt.foldMap(IOEval).value shouldBe Option(123)
      print("Write anything that's not a number: ")
      toInt.foldMap(IOEval).value shouldBe Option.empty

      println("Write `hello` and then `world`:")
      doubleRead.foldMap(IOEval).value shouldBe "helloworld"
    }
  }

  // And for test purposes we give a pure instance that works with
  // the `MonadState` algebra.
  object StateInstance {
    import GenericPrograms._

    case class IOState(readList: List[String], writeList: List[String]) {
      def read: IOState = this.copy(readList = readList.tail)
      def write(s: String): IOState = this.copy(writeList = s :: writeList)
    }

    type IOAction[A] = State[IOState, A]

    object IOAction extends ~>[IOF, IOAction] {
      val monadState = implicitly[MonadState[IOAction, IOState]]
      import monadState._

      def apply[A](fa: IOF[A]): IOAction[A] = fa match {
        case Read =>
          for {
            h <- inspect(_.readList.head)
            _ <- modify(_.read)
          } yield h
        case Write(msg) =>
          modify(_.write(msg))
      }
    }

    // Usually we want to write tests for a pure interpretation like this one
    // We can verify every input, output and result.
    object Test {
      echo.foldMap(IOAction).run(IOState("hello!" :: Nil, Nil)).value shouldBe
        (IOState(Nil, "hello!" :: Nil), ())

      askName.foldMap(IOAction).run(IOState("Javier Fuentes" :: Nil, Nil)).value shouldBe
        (IOState(Nil, "What's your name?" :: Nil), "Javier Fuentes")

      toInt.foldMap(IOAction).run(IOState("123" :: Nil, Nil)).value shouldBe
        (IOState(Nil, Nil), Option(123))
      toInt.foldMap(IOAction).run(IOState("This is not an Integer" :: Nil, Nil)).value shouldBe
        (IOState(Nil, Nil), Option.empty)

      doubleRead.foldMap(IOAction).run(IOState("Reading 1" :: "Reading 2" :: Nil, Nil)).value shouldBe
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

object FreeMonad extends FreeMonad