package org.hablapps.gist

import org.scalatest._
import scalaz._, Scalaz._

/**
 * Store comonad
 */
case class Store[A, S](index: A, f: A => S){
  def copointTo(s: A): Store[A, S] =
    Store(s, f)

  def apply(indexes: A*): Seq[S] =
    indexes.map(f)
}

object Store{

  implicit def ComonadStore[A] = new Comonad[Store[A,?]]{
    def map[S, T](store: Store[A, S])(f: S => T): Store[A, T] =
      Store(store.index, store.f andThen f)

    def cobind[S, T](store: Store[A, S])(f: Store[A, S] => T): Store[A, T] =
      Store(store.index, (a: A) => f(Store(a, store.f)) : T)

    def copoint[S](store: Store[A, S]): S =
      store.f(store.index)
  }
}

class StoreSpec extends FunSpec with Matchers{

  describe("Store comonad"){

    val habla: Map[Int,String] =
      Map(1->"juan", 2->"jesus", 3->"javi")

    it("must map functions"){

      def lengthStoreUsingCobind[F[_]: Functor, A]: Store[A, F[String]] => Store[A, F[Int]] =
        _ cobind {
          case Store(index, f) => f(index).map(_.length)
        }

      def lengthStore[F[_]: Functor, A]: Store[A, F[String]] => Store[A, F[Int]] =
        _ map ( _.map( _.length ) )

      val result: Store[Int, Option[Int]] =
        lengthStore[Option, Int].apply(Store[Int, Option[String]](1, habla.get))

      result.copoint shouldBe Some(4)

      result(0, 1, 2, 3, 4) shouldBe
        Seq(None, Some(4), Some(5), Some(4), None)
    }

    it("must work with more complex, contextual, mappings: cobind"){

      def strangeLength[F[_]: Apply](store: Store[Int, F[String]]): Store[Int, F[Int]] =
        store cobind {
          case Store(index, f) =>
            (f(index-1) |@| f(index) |@| f(index+1)){
              (a,b,c) => (a+b+c).length
            }
        }

      strangeLength(Store(4, habla.get)).apply(0, 1, 2, 3, 4) shouldBe
        Seq(None, None, Some(13), None, None)
    }
  }
}
