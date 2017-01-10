package org.hablapps.gist
package hello

import scala.io.StdIn.readLine

// Say what?
object Step2 {

  /* Impure program */
  def sayWhat: String = readLine

  /* Functional solution */
  object Fun {

    // Language
    type IOProgram[A] = IOEffect[A]

    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program
    def pureSayWhat: IOProgram[String] = Read

    // Interpreter
    def run[A](program: IOProgram[A]): A =
      program match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition
    def sayWhat: String = run(pureSayWhat)

  }

}

// Say what? (reloaded)
object Step3 {

  /* Impure program */
  def helloSayWhat: String = {
    println("Hello, say something:")
    readLine
  }

  /* Functional solution */
  object Fun {

    // Language
    sealed trait IOProgram[A]
    case class Single[A](e: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p1: IOProgram[A], p2: IOProgram[B]) extends IOProgram[B]

    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program
    def pureHelloSayWhat: IOProgram[String] =
      Sequence(
        Single(Write("Hello, say something:")),
        Single(Read))

    // Interpreter
    def run[A](program: IOProgram[A]): A =
      program match {
        case Single(e) => runEffect(e)
        case Sequence(p1, p2) =>
          run(p1)
          run(p2)
      }

    def runEffect[A](effect: IOEffect[A]): A =
      effect match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition
    def helloSayWhat: String = run(pureHelloSayWhat)

  }

}

// Echo, echo!
object Step4 {

  /* Impure program */
  def echo: Unit = {
    val read: String = readLine
    println(read)
  }

  /* Functional solution */
  object Fun {

    // Language
    sealed trait IOProgram[A]
    case class Single[A](e: IOEffect[A]) extends IOProgram[A]
    case class Sequence[A, B](p1: IOProgram[A], p2: A => IOProgram[B]) extends IOProgram[B]

    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program
    def pureEcho: IOProgram[Unit] =
      Sequence(
        Single(Read), (read: String) =>
        Single(Write(read)))

    // Interpreter
    def run[A](program: IOProgram[A]): A =
      program match {
        case Single(e) => runEffect(e)
        case Sequence(p1, p2) =>
          val res1 = run(p1)
          run(p2(res1))
      }

    def runEffect[A](effect: IOEffect[A]): A =
      effect match {
        case Write(msg) => println(msg)
        case Read => readLine
      }

    // Composition
    def echo: Unit = run(pureEcho)

  }

}

// On pure values
object Step5 {

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

    sealed trait IOEffect[A]
    case class Write(s: String) extends IOEffect[Unit]
    case object Read extends IOEffect[String]

    // Program
    def pureEcho: IOProgram[String] =
      Sequence(
        Single(Read), (read: String) =>
        Sequence(
          Single(Write(read)), (_: Unit) =>
          Value(read)
        )
      )

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
