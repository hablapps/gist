package org.hablapps.gist

import org.scalatest._

/**
Some tests for the `monad` macro. Run these tests as follows:

  test-only org.hablapps.gist.MonadMacro

*/
class MonadMacro extends FunSpec with Matchers with Inside{
  import cats.Monad

  /** 
   This is a simple program with just one "return" instruction.
   */
  describe("Simple pure translation"){

    def test[P[_]: Monad](i: Int): P[Int] = monad{
      i + 1
    }

    it("should work with Option"){
      import cats.instances.option._
      test[Option](2) shouldBe Some(3)
    }

    it("should work with Id"){
      import cats.Id
      test[Id](2) shouldBe 3
    }
  }


  /** 
   Simple program with several nested `flatMap`s
   */
  describe("Several simple flatMaps"){

    def test[P[_]: Monad](i: Int): P[Int] = monad{
      val s: String = "2"
      val j: Int = s.length + i
      j+1
    }

    it("should work with Option"){
      import cats.instances.option._
      test[Option](2) shouldBe Some(4)
    }

    it("should work with Id"){
      import cats.Id
      test[Id](2) shouldBe 4
    }

    it("should work with reified programs"){
      
      abstract class Program[_]
      case class Returns[A](a: A) extends Program[A]
      case class DoAndThen[A,B](a: Program[A],
        f: A => Program[B]) extends Program[B]

      object Program{
        implicit val M = new Monad[Program]{
          def pure[A](a: A) = Returns(a)
          def flatMap[A,B](p: Program[A])(f: A => Program[B]) =
            DoAndThen(p,f)
          def tailRecM[A,B](a: A)(f: A => Program[Either[A,B]]) = ???
        }
      }
      
      inside(test[Program](2)) {
        case DoAndThen(Returns("2"), f) =>
          inside(f("2")) {
            case DoAndThen(Returns(3), g) =>
              g(3) shouldBe Returns(4)
          }
      }
    }
  }

  /** 
  What if our pure function has to deal with programs?
  Then, we simulate their execution using a fake `run` method.
  */
  describe("Simple example with .run"){

    import monad._

    def test[P[_]: Monad](p1: P[String], p2: P[Int]): P[Int] = monad{
      val i: String = p1.run
      val j: Int = p2.run
      i.length + j
    }

    import cats.instances.option._

    it("should work with Option"){
      test[Option](Some("ab"),Some(1)) shouldBe Some(3)
    }

  }

  /** 
   If our monadic program needs access to instructions
   of particular APIs (which is the normal case), we can 
   also use the `.run` trick.
   */
  describe("Monadic programs over particular APIs"){
    import cats.Id

    // IO Algebra

    trait IO[P[_]]{
      def read(): P[String]
      def write(msg: String): P[Unit]
    }

    object IO{
      object Syntax{
        def read[P[_]]()(implicit IO: IO[P]) = IO.read()
        def write[P[_]](msg: String)(implicit IO: IO[P]) = IO.write(msg)
      }

      // Side-effectful interpretation
      implicit object IOId extends IO[Id]{
        def read() = scala.io.StdIn.readLine()
        def write(msg: String) = println(msg)
      }
    }

    // Simple state transformation for purely functional testing

    case class IOState(toBeRead: List[String], written: List[String])

    object IOState{
      import cats.data.State

      type Action[T] = State[IOState,T]

      implicit object IOAction extends IO[Action]{
        def read(): Action[String] =
          for {
            s <- State.get
            _ <- State.set(s.copy(toBeRead = s.toBeRead.tail))
          } yield s.toBeRead.head

        def write(msg: String): Action[Unit] =
          State.modify{ s =>
            s.copy(written = msg :: s.written)
          }
      }
    }

    // Sample program

    import IO.Syntax._, monad._

    def test[P[_]: Monad: IO](): P[String] = monad{
      val msg: String = read().run
      val _ : Unit = write(msg).run
      msg
    }

    it("should work with Id"){
      // Uncomment to be prompted at the console
      // test[Id]() shouldBe "hi!"
    }

    it("should work with State"){
      test[IOState.Action]().run(IOState(List("hi!"),List())).value shouldBe
        (IOState(List(),List("hi!")),"hi!")
    }

    it("should work when no ValDef is used as well"){
      
      def test2[P[_]: Monad: IO](): P[String] = monad{
        val msg: String = read().run
        write(msg).run
        msg
      }
      
      test2[IOState.Action]().run(IOState(List("hi!"),List())).value shouldBe
        (IOState(List(),List("hi!")),"hi!")
    }    
  }


}