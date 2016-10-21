package org.hablapps.gist

import org.scalatest._

object StateOptics extends FlatSpec with Matchers {

  object Optics {

    import scalaz.{ Reader, State }

    type IOCoalgebra[IOAlg[_[_]], Step[_, _], S] = IOAlg[Step[S, ?]]

    trait LensAlg[A, P[_]] {
      def get: P[A]
      def set(a: A): P[Unit]
    }

    type Lens[S, A] = IOCoalgebra[LensAlg[A, ?[_]], State, S]

    trait OptionalAlg[A, P[_]] {
      def getOption: P[Option[A]]
      def set(a: A): P[Unit]
    }

    type Optional[S, A] = IOCoalgebra[OptionalAlg[A, ?[_]], State, S]

    trait SetterAlg[A, P[_]] {
      def set(a: A): P[Unit]
    }

    type Setter[S, A] = IOCoalgebra[SetterAlg[A, ?[_]], State, S]

    trait GetterAlg[A, P[_]] {
      def get: P[A]
    }

    type Getter[S, A] = IOCoalgebra[GetterAlg[A, ?[_]], Reader, S]
  }

  object OpticsVsState {

    import scalaz.{ Equal, Functor, Monad, State }
    import scalaz.syntax.id._
    import scalaz.syntax.monad._

    import Optics.{ IOCoalgebra, LensAlg }

    // MonadState & Laws (Testing Typeclass Laws - Jeuring)

    trait MonadState[F[_], S] extends Monad[F] {
      def get: F[S]
      def put(s: S): F[Unit]
      def gets[A](f: S => A): F[A] = map(get)(f)
      def modify(f: S => S): F[Unit] = bind(get)(s => put(f(s)))
    }

    object MonadState {

      def PutPut[F[_], S](
          s1: S,
          s2: S)(implicit
          FU: Equal[F[Unit]],
          MS: MonadState[F, S]) =
        FU.equal(MS.put(s1) >> MS.put(s2), MS.put(s2))

      def PutGet[F[_], S](
          s: S)(implicit
          FS: Equal[F[S]],
          MS: MonadState[F, S]) =
        FS.equal(MS.put(s) >> MS.get, MS.put(s) >> s.point)

      def GetPut[F[_], S](implicit
          FU: Equal[F[Unit]],
          MS: MonadState[F, S]) =
        FU.equal(MS.get >>= MS.put, ().point)

      def GetGet[F[_], S](implicit
          FSS: Equal[F[(S, S)]],
          MS: MonadState[F, S]) =
        FSS.equal(
          MS.get >>= (s1 => MS.get >>= (s2 => (s1, s2).point)),
          MS.get >>= (s => (s, s).point))
    }

    // Derived methods for LensAlg

    def gets[A, F[_], B](f: A => B)(implicit
        L: LensAlg[A, F],
        F: Functor[F]): F[B] =
      L.get map f

    def modify[A, F[_]](f: A => A)(implicit
        L: LensAlg[A, F],
        M: Monad[F]): F[Unit] =
      L.get >>= (f andThen L.set)

    // Lenses Laws (A Clear Picture of Lens Laws - Fischer)

    class VeryWellBehavedLensLaws[S, A](LN: Lens[S, A]) {

      // Nicer notation

      def get: S => A = LN.get.eval

      def put(a: A): S => S = LN.set(a).exec

      // Laws

      def PutPut(s: S, v1: A, v2: A) =
        put(v2)(put(v1)(s)) == put(v1)(s)
        // (LN.set(v1) >> LN.set(v2)).exec(s) == LN.set(v2).exec(s)

      def PutGet(s: S, v: A) =
        get(put(v)(s)) == v
        // (LN.set(v) >> LN.get).eval(s) == v

      def GetPut(s: S) =
        put(get(s))(s) == s
        // (LN.get >>= LN.set).exec(s) == s

      // bonus!
      def GetGet(s: S) =
        (get(s), get(s)) == get(s) |> (v => (v, v))
        // (LN.get >>= (s1 => (LN.get >>= (s2 => (s1, s2).point[State[S, ?]])))).eval(s) ==
        //   (LN.get >>= (v => (v, v).point[State[S, ?]])).eval(s)
    }

    // Using Lens

    type Lens[S, A] = IOCoalgebra[LensAlg[A, ?[_]], State, S]

    case class Person(name: String, age: Int)

    implicit val ln: Lens[Person, String] = new LensAlg[String, State[Person, ?]] {
      def get: State[Person, String] = State.get map (_.name)
      def set(_name: String): State[Person, Unit] =
        State.modify(_.copy(name = _name))
    }

    val p1 = gets[String, State[Person, ?], String](_.toUpperCase)
    p1.run(Person("jesus", 30))

    // Using State

    type StateLens[S] = Lens[S, S]

    implicit val st: StateLens[Person] = new LensAlg[Person, State[Person, ?]] {
      def get: State[Person, Person] = State.get
      def set(p: Person): State[Person, Unit] = State.put(p)
    }

    val p2 = modify[Person, State[Person, ?]](p => p.copy(age = p.age + 1))
    p2.run(Person("jesus", 30))
  }

  object Monocle {

  }
}
