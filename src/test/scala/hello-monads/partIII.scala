package org.hablapps.gist
package hello

import scala.io.StdIn.readLine

// Sugar (Smart constructors)
object Step6 {

  /* Impure program */
  def echo: String = {
    val read: String = readLine
    println(read)
    read
  }

  /* Functional solution */
  object Fun {

    // Language
    sealed trait IOProgram[A]
    case class Single[A](e: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p1: IOProgram[A], p2: A => IOProgram[B]) extends IOProgram[B]
    case class Value[A](a: A) extends IOProgram[A]

    object IOProgram {
      object syntax {
        val readL: IOProgram[String] = Single(Read)
        def writeL(msg: String): IOProgram[Unit] = Single(Write(msg))
      }
    }

    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program
    import IOProgram.syntax._
    def pureEcho: IOProgram[String] =
      Sequence(
        readL, (read: String) =>
        Sequence(
          writeL(read), (_: Unit) =>
          Value(read)))

    // Interpreter
    def run[A](program: IOProgram[A]): A =
      program match {
        case Single(e) => runEffect(e)
        case Sequence(p1, p2) =>
          val res1 = run(p1)
          run(p2(res1))
        case Value(a) => a
      }

    def runEffect[A](effect: IOEffect[A]): A =
      effect match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition
    def echo: String = run(pureEcho)

  }

}

// Sugar (For-comprehensions)
object Step7 {

  /* Impure programs */
  def hello: Unit = println("Hello, world!")

  def sayWhat: String = readLine

  def helloSayWhat: String = {
    println("Hello, say something:")
    readLine
  }

  def echo1: Unit = {
    val read: String = readLine
    println(read)
  }

  def echo2: String = {
    val read: String = readLine
    println(read)
    read
  }

  /* Functional solution */
  object Fun {

    // Language
    sealed trait IOProgram[A] {
      def flatMap[B](f: A => IOProgram[B]) = Sequence(this, f)
      def map[B](f: A => B) = flatMap(f andThen Value.apply)
    }
    case class Single[A](e: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p1: IOProgram[A], p2: A => IOProgram[B]) extends IOProgram[B]
    case class Value[A](a: A) extends IOProgram[A]

    object IOProgram {
      object syntax {
        val readL: IOProgram[String] = Single(Read)
        def writeL(msg: String): IOProgram[Unit] = Single(Write(msg))
      }
    }

    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Pure Programs
    import IOProgram.syntax._

    val pureHello: IOProgram[Unit] = writeL("Hello, world!")

    val pureSayWhat: IOProgram[String] = readL

    def pureHelloSayWhat: IOProgram[String] =
      for {
        _ <- writeL("Hello, say something:")
        read <- readL
      } yield read

    def pureEcho1: IOProgram[Unit] =
      for {
        read <- readL
        _ <- writeL(read)
      } yield ()

    def pureEcho2: IOProgram[String] =
      for {
        read <- readL
        _ <- writeL(read)
      } yield read

  }

}

// Let it Free
object Step8 {

  /* Impure program */

  def echo: String = {
    val read: String = readLine
    println(read)
    read
  }

  /* Functional solution */
  object Fun {

    // Free Monad!
    sealed trait Program[F[_], A] {
      def flatMap[B](f: A => Program[F, B]) = Sequence(this, f)
      def map[B](f: A => B) = flatMap(f andThen Value.apply[F, B])
    }
    case class Single[F[_], A](e: F[A]) extends Program[F, A]
    case class Sequence[F[_], A, B](p1: Program[F, A], p2: A => Program[F, B]) extends Program[F, B]
    case class Value[F[_], A](a: A) extends Program[F, A]

    // Interpreter
    trait RunEffect[F[_]] {
      def apply[A](fa: F[A]): A
    }

    def run[F[_], A](program: Program[F, A])(runEffect: RunEffect[F]): A =
      program match {
        case Single(e) => runEffect(e)
        case Sequence(p1, p2) =>
          val res1 = run(p1)(runEffect)
          run(p2(res1))(runEffect)
        case Value(a) => a
      }

    // Language
    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    type IOProgram[A] = Program[IOEffect, A]
    object IOProgram {
      object syntax {
        val readL: IOProgram[String] = Single(Read)
        def writeL(msg: String): IOProgram[Unit] = Single(Write(msg))
      }
    }

    object IOInterpreter extends RunEffect[IOEffect] {
      def apply[A](effect: IOEffect[A]): A =
        effect match {
          case Write(msg) => println(msg)
          case Read => readLine
        }
    }

    // Pure Programs
    import IOProgram.syntax._

    def pureEcho: IOProgram[String] =
      for {
        read <- readL
        _ <- writeL(read)
      } yield read

    // Composition
    def echo: String = run(pureEcho)(IOInterpreter)

  }

}

// Let it Free (revisited)
object Step9 {

  /* Impure program */

  def echo: String = {
    val read: String = readLine
    println(read)
    read
  }

  /* Functional solution */
  object Fun {

    // Auxiliary types
    type Id[A] = A

    trait Monad[F[_]] {
      def pure[A](a: A): F[A]
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    }

    implicit object idMonad extends Monad[Id] {
      def pure[A](a: A) = a
      def bind[A, B](fa: A)(f: A => B) = f(fa)
    }

    // Free Monad!
    sealed trait Program[F[_], A] {
      def flatMap[B](f: A => Program[F, B]) = Sequence(this, f)
      def map[B](f: A => B) = flatMap(f andThen Value.apply[F, B])
    }
    case class Single[F[_], A](e: F[A]) extends Program[F, A]
    case class Sequence[F[_], A, B](p1: Program[F, A], p2: A => Program[F, B]) extends Program[F, B]
    case class Value[F[_], A](a: A) extends Program[F, A]

    // Interpreter
    trait NaturalTransformation[F[_], G[_]] {
      def apply[A](fa: F[A]): G[A]
    }

    def run[F[_], A](program: Program[F, A])(natTrans: NaturalTransformation[F, Id]): A =
      foldMap(program)(natTrans)

    def foldMap[F[_], G[_], A](program: Program[F, A])(natTrans: NaturalTransformation[F, G])(implicit M: Monad[G]): G[A] =
      program match {
        case Single(e) => natTrans(e)
        case Sequence(p1, p2) =>
          M.bind(foldMap(p1)(natTrans)) { i =>
            foldMap(p2(i))(natTrans)
          }
        case Value(a) => M.pure(a)
      }

    // Language
    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    type IOProgram[A] = Program[IOEffect, A]
    object IOProgram {
      object syntax {
        val readL: IOProgram[String] = Single(Read)
        def writeL(msg: String): IOProgram[Unit] = Single(Write(msg))
      }
    }

    object IOInterpreter extends NaturalTransformation[IOEffect, Id] {
      def apply[A](effect: IOEffect[A]): A =
        effect match {
          case Write(msg) => println(msg)
          case Read => readLine
        }
    }

    // Pure Programs
    import IOProgram.syntax._

    def pureEcho: IOProgram[String] =
      for {
        read <- readL
        _ <- writeL(read)
      } yield read

    // Composition
    def echo: String = run(pureEcho)(IOInterpreter)
    def echo2: String = foldMap(pureEcho)(IOInterpreter)

  }

}
