package org.hablapps.gist

import org.scalatest._
import scala.language.existentials
import cats.{~>, Id}
import cats.data.{Const, State, Writer, WriterT}
import cats.evidence.Is

/*
The purpose of this gist is to show how can we implement non-compositional interpreters
for monadic programs using pattern matching, without using GADTs, i.e. free monads and 
similar representations. Instead, we will use Church encodings with higher-kinded algebras. 

This gist closely follows the structure of this other one, where the same claim is made
for regular ADTs (non GADTs):

https://github.com/hablapps/gist/blob/master/src/test/scala/ChurchEncodings.scala

In this gist, however, we'll use algebras right from the beginning. Also related, the
following gist illustrates the concept of initial algebra:

https://github.com/hablapps/gist/blob/master/src/test/scala/InitialAlgebras.scala

Last, as a follow up of this gist, you may want to have a look to this other one, which 
shows the equivalence between the Church representation explained here, and other GADT-based 
representations:

https://github.com/hablapps/gist/blob/hablacats/src/test/scala/IsomorphismsHK.scala
*/
class HKChurchEncodings extends FlatSpec with Matchers{

  /*
  An algebra for type constructors which represent monadic IO programs.
  */
  trait IOAlg[E[_]] {
    def write(msg: String): E[Unit]
    def read: E[String]
    def flatMap[A, B](p: E[A])(f: A => E[B]): E[B]
    def pure[A](a: A): E[A]
  }

  object IOAlg {

    object Syntax{
      def read[E[_]](implicit alg: IOAlg[E]): E[String] = alg.read
      def write[E[_]](msg: String)(implicit alg: IOAlg[E]): E[Unit] = alg.write(msg)
      def flatMap[E[_], A, B](p: E[A])(f: A => E[B])(implicit alg: IOAlg[E]): E[B] = alg.flatMap(p)(f)
      def pure[E[_], A](a: A)(implicit alg: IOAlg[E]): E[A] = alg.pure(a)
    }

  }

  /*
  Using the algebra alone, we can represent in a very abstract way different IO programs
  */
  import IOAlg.Syntax._

  def e1[F[_]: IOAlg]: F[Unit] = 
    flatMap(read[F])(write[F])

  def e2[F[_]: IOAlg]: F[String] = 
    flatMap(e1)(_ => read)

  def e3[F[_]: IOAlg]: F[String] =
    flatMap(read) { s1 =>
      flatMap(flatMap(read)(s2 => write(s1+s2)))(_ => read)
    }

  /*
  Initial algebras for type constructors. We will need it later. It's instructive to compare this 
  implementation with this other one: 
  
    https://github.com/hablapps/gist/blob/master/src/test/scala/InitialAlgebras.scala#L142
  */
  trait InitialAlgebraHK[F[_], Alg[_[_]]] {
    def algebra: Alg[F]
    def fold[G[_]: Alg]: F ~> G
  }

  /*
  Church encodings of IO programs. We show that this representation provides a canonical way
  of constructing IO programs, i.e. Church encodings are carriers of initial algebras.
  */
  object Church {

    /*
    Essentially, a Church encoding for IO programs is a reification of the polymorphic functions
    implemented for `e1`, `e2` and `e3` above.
    */
    trait IO[A] {
      def apply[F[_]](implicit alg: IOAlg[F]): F[A]
    }

    object IO extends InitialAlgebraHK[IO, IOAlg]{
      
      implicit val algebra = new IOAlg[IO] {
        def read: IO[String] =
          new IO[String] {
            def apply[F[_]](implicit alg: IOAlg[F]): F[String] =
              alg.read
          }
        def write(msg: String): IO[Unit] =
          new IO[Unit] {
            def apply[F[_]](implicit alg: IOAlg[F]): F[Unit] =
              alg.write(msg)
          }
        def flatMap[A, B](p: IO[A])(f: A => IO[B]): IO[B] =
          new IO[B] {
            def apply[F[_]](implicit alg: IOAlg[F]): F[B] =
              alg.flatMap[A, B](p.apply[F])(a => f(a).apply[F])
          }
        def pure[A](a: A): IO[A] =
          new IO[A] {
            def apply[F[_]](implicit alg: IOAlg[F]): F[A] =
              alg.pure(a)
          }
      }

      def fold[G[_]: IOAlg]: IO ~> G =
        new (IO ~> G) {
          def apply[A](fa: IO[A]): G[A] = fa.apply[G]
        }

    }
  }

  /*
  We can implement straightforwardly compositional interpreters using Church
  encodings. 
  */
  object CompositionalInterpreters{
    import Church._

    // Side-effectful interpretation of IO programs
    
    def run[A](program: IO[A]): A = 
      //program(IdAlg)
      // Equivalently:
      IO.fold(IdAlg)(program)

    val IdAlg = new IOAlg[Id] {
      def read: String = scala.io.StdIn.readLine
      def write(msg: String): Unit = println(msg)
      def flatMap[A, B](p: A)(f: A => B): B = f(p)
      def pure[A](a: A): A = a
    }

    // (Approximate) representation of IO programs as strings
    
    def write[A](program: IO[A]): String = 
      program(WriterAlg).written

    object WriterAlg extends IOAlg[Writer[String, ?]] {
      implicit val stringMonoid = new cats.kernel.instances.StringMonoid
      val monad = WriterT.catsDataMonadWriterForWriterT[Id, String]
      import monad._      

      def read: Writer[String, String] =
        writer(("Read", ""))
      def write(msg: String): Writer[String, Unit] =
        tell(s"Write")
      def flatMap[A, B](p: Writer[String, A])(f: A => Writer[String, B]): Writer[String, B] =
        for {
          _ <- tell(s"FlatMap(")
          a <- p
          _ <- tell(", ")
          b <- f(a)
          _ <- tell(")")
        } yield b
      def pure[A](a: A): Writer[String, A] =
        writer((s"Pure($a)", a))
    }

  }

  "Compositional interpreters" should "work" in {
    import CompositionalInterpreters._, Church._

    write(e3(IO.algebra)) shouldBe 
      "FlatMap(Read, FlatMap(FlatMap(Read, Write), Read))"

    // Equivalently
    e3(WriterAlg).written shouldBe write(e3(IO.algebra))
  }
      
  /*
  Non-compositional interpreters are more difficult to implement, since pattern 
  matching is not directly available. Nonetheless, we can get around it!
  */
  object DeconstructingChurch{
    import Church._

    /*
    This trait represents the possible cases that we can find when pattern
    matching an IO program, along with the functionality that we want to 
    enact for each case. The possible cases are: 1) our IO Program is a simple
    read instruction which return a string; 2) it's a simple writing instruction
    which eventually returns a unit value; 3) it's a composition of a generic IO 
    program that returns `A`, and another generic IO program that returns `B`; 
    and 4) it's a simple generic IO program that returns a value of type `B`. 
    Note that cases 3) and 4) deal with generic programs. That's why our `Cases`
    trait is generic as well: the parameter `B` represents the type of the 
    return value of our program, whereas `A` represents the parameters of the
    first program in case 3). Since this parameter is of no use in the other
    cases, we request evidence that this parameter is `Nothing` in those cases.
    */
    trait Cases[W[_], A, B]{
      def dread(implicit
        ev1: String Is B,
        ev2: Nothing Is A): W[B]
  
      def dwrite(msg: String)(implicit
        ev1: Unit Is B,
        ev2: Nothing Is A): W[B]
  
      def dflatMap(p: IO[A])(f: A => IO[B]): W[B]
  
      def dpure(a: B)(implicit ev: Nothing Is A): W[B]
    }

    object Cases {
      import IO.algebra._

      /*
      This will be used later when we want to reconstruct an IO program from
      its deconstruction
      */
      def Cons[X, Y] = new Cases[IO, X, Y]{
        def dread(implicit
            ev1: String Is Y,
            ev2: Nothing Is X): IO[Y] =
          ev1.substitute[IO](read)
  
        def dwrite(msg: String)(implicit
            ev1: Unit Is Y,
            ev2: Nothing Is X): IO[Y] =
          ev1.substitute[IO](write(msg))
  
        def dflatMap(p: IO[X])(f: X => IO[Y]): IO[Y] =
          flatMap(p)(f)
  
        def dpure(a: Y)(implicit ev: Nothing Is X): IO[Y] =
          pure(a)
      }
    }

    /*
    The trait `Cases` represents the possible deconstructions of an IO program. 
    This trait `Match` represent an actual case. 
    */
    trait Match[B] {
      type I
      def apply[W[_]](cases: Cases[W, I, B]): W[B]
    }

    object Match {

      type Aux[A, B] = Match[B] { type I = A }

      // In order to obtain the actual case of an IO program we have no choice
      // but design an algebra of `Match` values.

      def apply[B](e: IO[B]): Match[B] =
        e(Alg)

      val Alg: IOAlg[Match] = new IOAlg[Match] {
        def read: Match.Aux[Nothing, String] = new Match[String] {
          type I = Nothing
          def apply[W[_]](cases: Cases[W, I, String]): W[String] = cases.dread
        }
        def write(msg: String): Match.Aux[Nothing, Unit] = new Match[Unit] {
          type I = Nothing
          def apply[W[_]](cases: Cases[W, I, Unit]): W[Unit] = cases.dwrite(msg)
        }
        def flatMap[A2, B](p: Match[A2])(f: A2 => Match[B]): Match.Aux[A2, B] =
          new Match[B] {
            type I = A2
            def apply[W[_]](cases: Cases[W, I, B]): W[B] =
              cases.dflatMap(p(Cases.Cons))(a => f(a)(Cases.Cons))
          }
        def pure[B](a: B): Match.Aux[Nothing, B] =
          new Match[B] {
            type I = Nothing
            def apply[W[_]](cases: Cases[W, I, B]): W[B] = cases.dpure(a)
          }
      }
    }
  }

  /*
  We can already pattern match Church encodings with the facilities implemented above,
  but with the following extractors we'll get more idiomatic implementations.
  */
  object ScalaExtractors{
    import Church._, DeconstructingChurch._

    object Read {
      def unapply[B](io: IO[B]): Option[Unit] = {
        val m = Match(io)
        m(new Cases[Const[Option[Unit], ?], m.I, B] {
          def dread(implicit ev1: String Is B, ev2: Nothing Is m.I): Const[Option[Unit], B] =
            Const(Some(()))
          def dwrite(msg: String)(implicit ev1: Unit Is B, ev2: Nothing Is m.I): Const[Option[Unit], B] =
            Const(None)
          def dflatMap(p: IO[m.I])(f: m.I => IO[B]): Const[Option[Unit], B] =
            Const(None)
          def dpure(a: B)(implicit ev: Nothing Is m.I): Const[Option[Unit], B] =
            Const(None)
        }).getConst
      }
    }

    object Write {
      def unapply[B](io: IO[B]): Option[String] = {
        val m = Match(io)
        m(new Cases[Const[Option[String], ?], m.I, B] {
          def dread(implicit ev1: String Is B, ev2: Nothing Is m.I): Const[Option[String], B] =
            Const(None)
          def dwrite(msg: String)(implicit ev1: Unit Is B, ev2: Nothing Is m.I): Const[Option[String], B] =
            Const(Some(msg))
          def dflatMap(p: IO[m.I])(f: m.I => IO[B]): Const[Option[String], B] =
            Const(None)
          def dpure(a: B)(implicit ev: Nothing Is m.I): Const[Option[String], B] =
            Const(None)
        }).getConst
      }
    }

    object FlatMap {
      type FlatMapPattern[A, B] = Option[(IO[A], A => IO[B])]
      def unapply[B](io: IO[B]): FlatMapPattern[_, B] = {
        val m = Match(io)
        m(new Cases[Const[FlatMapPattern[m.I, B], ?], m.I, B] {
          def dread(implicit
              ev1: String Is B,
              ev2: Nothing Is m.I): Const[FlatMapPattern[m.I, B],B] =
            Const(None)
          def dwrite(msg: String)(implicit
              ev1: Unit Is B,
              ev2: Nothing Is m.I): Const[FlatMapPattern[m.I, B],B] =
            Const(None)
          def dflatMap(p: IO[m.I])(f: m.I => IO[B]): Const[FlatMapPattern[m.I, B],B] =
            Const(Some((p, f)))
          def dpure(a: B)(implicit ev: Nothing Is m.I): Const[FlatMapPattern[m.I, B], B] =
            Const(None)
        }).getConst
      }
    }

    object Pure {
      def unapply[B](io: IO[B]): Option[B] = {
        val m = Match(io)
        m(new Cases[Const[Option[B], ?], m.I, B] {
          def dread(implicit ev1: String Is B, ev2: Nothing Is m.I): Const[Option[B], B] =
            Const(None)
          def dwrite(msg: String)(implicit ev1: Unit Is B, ev2: Nothing Is m.I): Const[Option[B], B] =
            Const(None)
          def dflatMap(p: IO[m.I])(f: m.I => IO[B]): Const[Option[B], B] =
            Const(None)
          def dpure(a: B)(implicit ev: Nothing Is m.I): Const[Option[B], B] =
            Const(Some(a))
        }).getConst
      }
    }
  }

  /*
  An example of non-compositional interpreter using pattern matching, as promised. 
  Unfortunately, the result is not so appealing due to some bugs of the Scala compiler
  related to pattern matching of existential types. Ideally, the code should be exactly
  like the one implemented for GADTs:

    https://github.com/hablapps/gist/blob/hablacats/src/test/scala/GADTs.scala#L128
  */
  object NonCompositionalInterpreters{
    import Church._, ScalaExtractors._

    type FMPattern[A, B] = (IO[A], A => IO[B])
    
    def reassociate[A](e: IO[A]): IO[A] = e match {
      case FlatMap(par: FMPattern[_, A]) =>
        import IO.algebra._
        par._1 match {
          case FlatMap(par2: FMPattern[_, _]) =>
            reassociate(flatMap(par2._1)(x => flatMap(par2._2(x))(par._2)))
          case other =>
            flatMap(reassociate(par._1))(x => reassociate(par._2(x)))
        }
      case other => other
    }

  }

  "Non-compositional interpreters" should "work" in {
    import Church._, CompositionalInterpreters._, NonCompositionalInterpreters._
    import IOAlg.Syntax.{flatMap, read}

    write(reassociate(e1(IO.algebra))) shouldBe
      write(e1(IO.algebra))
    
    write(reassociate(e2(IO.algebra))) shouldBe
      "FlatMap(Read, FlatMap(Write, Read))"
    
    write(reassociate(e3(IO.algebra))) shouldBe
      "FlatMap(Read, FlatMap(Read, FlatMap(Write, Read)))"

    write(reassociate(flatMap(e3(IO.algebra))((s: String) => read[IO]))) shouldBe
      "FlatMap(Read, FlatMap(Read, FlatMap(Write, FlatMap(Read, Read))))"
  }


}

object HKChurchEncodings extends HKChurchEncodings
