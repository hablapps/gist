package org.hablapps.gist

import org.scalatest._
import scalaz._, Scalaz._

class Comonads extends FunSpec with Matchers{

  /**
   * Store comonad
   */
  case class Store[S,A](f: S => A, index: S){
    def copointTo(s: S): Store[S,A] =
      Store(f,s)
    def apply(indexes: S*): Seq[A] =
      indexes.map(f)
  }

  object Store{

    implicit def ComonadStore[S] = new Comonad[Store[S,?]]{
      def map[A,B](store: Store[S,A])(f: A => B): Store[S,B] =
        Store(store.f andThen f, store.index)

      def cobind[A,B](store: Store[S,A])(f: Store[S,A] => B): Store[S,B] =
        Store((s: S) => f(Store(store.f,s)) : B, store.index)

      def copoint[A](store: Store[S,A]): A =
        store.f(store.index)
    }
  }


  describe("Store comonad"){

    val habla: Map[Int,String] = Map(1->"juan", 2->"jesus", 3->"javi")

    it("must map functions"){

      def lengthStoreUsingCobind[F[_]: Functor,S]: Store[S,F[String]] => Store[S,F[Int]] =
        _ cobind {
          case Store(f,index) => f(index).map(_.length)
        }

      def lengthStore[F[_]: Functor,S]: Store[S,F[String]] => Store[S,F[Int]] =
        _ map ( _.map( _.length ) )

      val result: Store[Int,Option[Int]] =
        lengthStore[Option,Int].apply(Store[Int,Option[String]](habla.get,1))

      result.copoint shouldBe Some(4)

      result(0, 1, 2, 3, 4) shouldBe
        Seq(None, Some(4), Some(5), Some(4), None)
    }

    it("must work with more complex, contextual, mappings: cobind"){

      def strangeLength[F[_]: Apply](store: Store[Int,F[String]]): Store[Int,F[Int]] =
        store cobind {
          case Store(f,index) => (f(index-1) |@| f(index) |@| f(index+1))((a,b,c) => (a+b+c).length)
        }

      strangeLength(Store(habla.get,4)).apply(0,1,2,3,4) shouldBe
        Seq(None,None,Some(13),None,None)

    }
  }

}