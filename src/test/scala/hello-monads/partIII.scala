package org.hablapps.gist
package hello

import scala.io.StdIn.readLine

/**
 * Code for the post 'From "Hello, world!" to "Hello, monad!"'' (part III/III)
 *
 * https://purelyfunctional.wordpress.com/?p=1195
 *
 * It continues the code for the first and second part of this series that
 * you can find in files ./partI.scala and ./partII.scala.
 */

// First objection: Readability

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

  // Program using `flatMap` and `map` operators

  object ProgramWithInfixOps{
    import IOProgram.Syntax._

    def echo(): IOProgram[String] =
      read() flatMap { msg =>
        write(msg) map { _ =>
          msg
        }
      }
  }

  // Program using for-comprehensions

  object ProgramWithForComprehensions{
    import IOProgram.Syntax._

    def echo(): IOProgram[String] = for{
      msg <- read()
      _ <- write(msg)
    } yield msg
  }


  // Interpreter: Doesn't change from previous designs

  import ProgramWithInfixOps._

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

  // Composition: doesn't change either

  def consoleEcho: String = run(echo())

}


// Modularity problems: Helo, Monad!
object Step7 {

  // Other imperative language
  object MonolithicDSL{

    sealed trait FileSystemProgram[A]
    case class Single[A](e: FileSystemProgram.Effect[A]) extends FileSystemProgram[A]
    case class Sequence[A, B](p1: FileSystemProgram[A], p2: A => FileSystemProgram[B]) extends FileSystemProgram[B]
    case class Value[A](a: A) extends FileSystemProgram[A]

    object FileSystemProgram{
      sealed abstract class Effect[_]
      case class ReadFile(path: String) extends Effect[String]
      case class DeleteFile(path: String) extends Effect[Unit]
      case class WriteFile(path: String, content: String) extends Effect[Unit]
    }
  }

  // Abstract imperative DSL

  sealed trait ImperativeProgram[Effect[_],A]{
    def flatMap[B](f: A => ImperativeProgram[Effect,B]): ImperativeProgram[Effect,B] =
      Sequence(this, f)
    def map[B](f: A => B): ImperativeProgram[Effect,B] =
      flatMap(f andThen Value.apply)
  }
  case class Single[Effect[_],A](e: Effect[A]) extends ImperativeProgram[Effect,A]
  case class Sequence[Effect[_],A, B](p1: ImperativeProgram[Effect,A],
    p2: A => ImperativeProgram[Effect,B]) extends ImperativeProgram[Effect,B]
  case class Value[Effect[_],A](a: A) extends ImperativeProgram[Effect,A]

  // Modular redefinition of IO programs

  type IOProgram[A] = ImperativeProgram[IOProgram.Effect, A]

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

  // Program: doesn't change at all!

  import IOProgram.Syntax._

  def echo(): IOProgram[String] = for{
    msg <- read()
    _ <- write(msg)
  } yield msg

}

