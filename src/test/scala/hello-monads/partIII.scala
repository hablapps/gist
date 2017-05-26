package org.hablapps.gist
package hello

import scala.io.StdIn.readLine

// Smart constructors
object Step6 {

  // Language

  sealed trait IOProgram[A]{
    def flatMap[B](f: A => IOProgram[B]): IOProgram[B] =
      Sequence(this, f)
    def map[B](f: A => B): IOProgram[B] =
      flatMap(f andThen Value.apply)
  }
  case class Single[A](e: IOProgram.Effect[A]) extends IOProgram[A]
  case class Sequence[A, B](p1: IOProgram[A],
    p2: A => IOProgram[B]) extends IOProgram[B]
  case class Value[A](a: A) extends IOProgram[A]

  object IOProgram{

    sealed trait Effect[A]
    case class Write(s: String) extends Effect[Unit]
    case object Read extends Effect[String]

    object Syntax{
      def read(): IOProgram[String] =
        Single(Read)

      def write(msg: String): IOProgram[Unit] =
        Single(Write(msg))
    }
  }

  // Program

  import IOProgram.Syntax._

  def echo: IOProgram[String] =
    read() flatMap { msg =>
      write(msg) map { _ =>
        msg
      }
    }

  // Interpreter
  def run[A](program: IOProgram[A]): A =
    program match {
      case Single(e) => runEffect(e)
      case Sequence(p1, p2) =>
        val res1 = run(p1)
        run(p2(res1))
      case Value(a) => a
    }

  def runEffect[A](effect: IOProgram.Effect[A]): A =
    effect match {
      case IOProgram.Write(msg) => println(msg)
      case IOProgram.Read => readLine
    }

  // Composition
  def consoleEcho: String = run(echo)

}
