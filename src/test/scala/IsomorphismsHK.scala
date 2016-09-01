package org.hablapps.gist

import cats.{~>, <=>, <~>, Monad}
import cats.data.Const
import cats.evidence.Is
import cats.free.Free

import HKChurchEncodings.IOAlg, HKChurchEncodings.InitialAlgebraHK

/*
IO programs can be represented as Church encodings, as Free monads, 
using ad-hoc GADTs, ... We show in this gist that these representations
are equivalent, exploting the fact that all of them are carriers of 
initial algebras. 

This gist is make reference to these other ones on GADTs and Church
encodings of IO programs: 

 - https://github.com/hablapps/gist/blob/hablacats/src/test/scala/GADTs.scala
 - https://github.com/hablapps/gist/blob/hablacats/src/test/scala/ChurchEncodingsHK.scala

*/
class HK_Isormophisms{

  /*
  We first show that the ad-hoc representation of IO programs using
  GADTs make up an initial algebra.
  */
  object ADTInitial extends InitialAlgebraHK[GADTs.IO, IOAlg]{
    import GADTs._

    val algebra = new IOAlg[IO] {
      def read: IO[String] = Read
      def write(msg: String): IO[Unit] = Write(msg)
      def flatMap[A, B](p: IO[A])(f: A => IO[B]): IO[B] = FlatMap(p, f)
      def pure[A](a: A): IO[A] = Pure(a)
    }

    // Here, we may reuse the `fold` version already implemented for
    // the IO GADT, but this is a more simple implementation.
    import IOAlg.Syntax._
    def fold[G[_]: IOAlg]: IO ~> G =
      new (IO ~> G) {
        def apply[A](fa: IO[A]): G[A] = fa match {
          case Read => read
          case Write(msg) => write(msg)
          case FlatMap(p, f) =>
            flatMap(fold.apply(p))(a => fold.apply(f(a)))
          case Pure(a) => pure(a)
        }
      }

    val e1: IO[Unit] = algebra.flatMap(algebra.read)(algebra.write)
  }

  /*
  That IO GADT and Church IO are equivalent is easy given the fact that
  both of them are carriers of initial algebras.
  */
  object ChurchStuff {
    import HKChurchEncodings.Church.IO
    
    val iso = new (GADTs.IO <~> IO) {

      def from: IO ~> GADTs.IO =
        IO.fold(ADTInitial.algebra)

      def to: GADTs.IO ~> IO =
        ADTInitial.fold(IO.algebra)

    }

    /* 
    Bonus: another isomorphism that shows the data type encoded by the `Match`
    trait for Church encodings
    */
    object MatchStuff {
      import HKChurchEncodings.DeconstructingChurch.{Match, Cases}

      sealed abstract class IOF[A, B]
      case object Read extends IOF[Nothing, String]
      case class Write(msg: String) extends IOF[Nothing, Unit]
      case class FlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IOF[A, B]
      case class Pure[A](a: A) extends IOF[Nothing, A]

      def iso[A, B] = new (IOF[A, B] <=> Match.Aux[A, B]) {

        def from: Match.Aux[A, B] => IOF[A, B] = m =>
          m(new Cases[Const[IOF[A, B], ?], A, B] {
            def dread(implicit
                ev1: String Is B,
                ev2: Nothing Is A): Const[IOF[A, B], B] = {
              val lifted = ev2.lift2[IOF, String, B](ev1)
              Const(lifted.coerce(Read))
            }
            def dwrite(msg: String)(implicit
                ev1: Unit Is B,
                ev2: Nothing Is A): Const[IOF[A, B], B] = {
              val lifted = ev2.lift2[IOF, Unit, B](ev1)
              Const(lifted.coerce(Write(msg)))
            }
            def dflatMap(p: IO[A])(f: A => IO[B]): Const[IOF[A, B], B] =
              Const(FlatMap(p, f))
            def dpure(b: B)(implicit ev: Nothing Is A): Const[IOF[A, B], B] = {
              val lifted = ev.lift2[IOF, B, B](implicitly)
              Const(lifted.coerce(Pure(b)))
            }
          }).getConst

        def to: IOF[A, B] => Match.Aux[A, B] = {
          case Read =>
            new Match[B] {
              type I = A
              def apply[W[_]](cases: Cases[W, I, B]): W[B] = cases.dread
            }
          case Write(s) =>
            new Match[B] {
              type I = A
              def apply[W[_]](cases: Cases[W, I, B]): W[B] = cases.dwrite(s)
            }
          case FlatMap(fa, f) =>
            new Match[B] {
              type I = A
              def apply[W[_]](cases: Cases[W, I, B]): W[B] = cases.dflatMap(fa)(f)
            }
          case Pure(a) =>
            new Match[B] {
              type I = A
              def apply[W[_]](cases: Cases[W, I, B]): W[B] = cases.dpure(a)
            }
        }
      }
    }
  }

  /*
  We can also show the equivalence between a Free representation of IO programs
  and their Church encodings. We may have shown as well that `IOFree` is a carrier of
  the initial IO algebra, and then construct the isomorphism as we did for IO GADT.
  */
  object FreeMonadStuff {
    import HKChurchEncodings.Church.IO

    /* Definition of IO programs in terms of Free monads*/

    sealed abstract class IOF[_]
    case object Read extends IOF[String]
    case class Write(msg: String) extends IOF[Unit]

    type IOFree[A] = Free[IOF, A]

    /* Free IO is isomorphic to Church IO */

    val iso = new (IOFree <~> IO) {

      def from: IO ~> IOFree = new (IO ~> IOFree) {
        def apply[A](fa: IO[A]): IOFree[A] = {
          val alg: IOAlg[IOFree] = new IOAlg[IOFree] {
            def read: IOFree[String] = Free.liftF(Read)
            def write(msg: String): IOFree[Unit] = Free.liftF(Write(msg))
            def flatMap[A, B](p: IOFree[A])(f: A => IOFree[B]): IOFree[B] = p flatMap f
            def pure[A](a: A): IOFree[A] = Free.pure(a)
          }
          fa(alg)
        }
      }

      def to: IOFree ~> IO = new (IOFree ~> IO) {
        def apply[A](fa: IOFree[A]): IO[A] = new IO[A] {
          def apply[F[_]](implicit alg: IOAlg[F]): F[A] = {
            val nat: IOF ~> F = new (IOF ~> F) {
              def apply[A](ioa: IOF[A]): F[A] = ioa match {
                case Read => alg.read
                case Write(msg) => alg.write(msg)
              }
            }
            val mon: Monad[F] = new Monad[F] {
              def pure[A](a: A): F[A] = alg.pure(a)
              def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = alg.flatMap(fa)(f)

              def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
                defaultTailRecM(a)(f)
            }
            fa.foldMapUnsafe(nat)(mon)
          }
        }
      }
    }

  }

}

object HK_Isormophisms extends HK_Isormophisms
