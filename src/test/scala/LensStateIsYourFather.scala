package org.hablapps.blog

import org.scalatest._

import scalaz.{ Reader, State }
import scalaz.Isomorphism.<=>

import monocle.{ Getter, Lens, Optional, Setter, Fold }

class LensStateIsYourFather extends FlatSpec with Matchers {

  type IOCoalgebra[IOAlg[_[_]], Step[_, _], S] = IOAlg[Step[S, ?]]

  object OpticsAsCoalgebras {
    import scalaz.{ Functor, Monad }
    import scalaz.syntax.monad._

    /* IOLens */

    trait IOLensAlg[A, P[_]] {
      def get: P[A]
      def set(a: A): P[Unit]

      def gets[B](
          f: A => B)(implicit
          F: Functor[P]): P[B] =
        get map f

      def modify(
          f: A => A)(implicit
          M: Monad[P]): P[Unit] =
        get >>= (f andThen set)
    }

    type IOLens[S, A] = IOCoalgebra[IOLensAlg[A, ?[_]], State, S]

    object IOLens {

      def apply[S, A](_get: S => A)(_set: A => S => S): IOLens[S, A] =
        new IOLensAlg[A, State[S, ?]] {
          def get: State[S, A] = State.gets(_get)
          def set(a: A): State[S, Unit] = State.modify(_set(a))
        }

      def lensIso[S, A] = new (Lens[S, A] <=> IOLens[S, A]) {

        def from: IOLens[S, A] => Lens[S, A] =
          ioln => Lens[S, A](ioln.get.eval)(a => ioln.set(a).exec)

        def to: Lens[S, A] => IOLens[S, A] = ln => new IOLens[S, A] {
          def get: State[S, A] = State.gets(ln.get)
          def set(a: A): State[S, Unit] = State.modify(ln.set(a))
        }
      }
    }

    /* IOOptional */

    trait IOOptionalAlg[A, P[_]] {
      def getOption: P[Option[A]]
      def set(a: A): P[Unit]
    }

    type IOOptional[S, A] = IOCoalgebra[IOOptionalAlg[A, ?[_]], State, S]

    object IOOptional {

      def optionalIso[S, A] = new (Optional[S, A] <=> IOOptional[S, A]) {

        def from: IOOptional[S, A] => Optional[S, A] =
          ioopt => Optional[S, A](ioopt.getOption.eval)(a => ioopt.set(a).exec)

        def to: Optional[S, A] => IOOptional[S, A] = opt => new IOOptional[S, A] {
          def getOption: State[S, Option[A]] = State.gets(opt.getOption)
          def set(a: A): State[S, Unit] = State.modify(opt.set(a))
        }
      }
    }

    /* IOSetter */

    trait IOSetterAlg[A, P[_]] {
      def modify(f: A => A): P[Unit]
    }

    type IOSetter[S, A] = IOCoalgebra[IOSetterAlg[A, ?[_]], State, S]

    object IOSetter {

      def setterIso[S, A] = new (Setter[S, A] <=> IOSetter[S, A]) {

        def from: IOSetter[S, A] => Setter[S, A] =
          iost => Setter[S, A](f => iost.modify(f).exec)

        def to: Setter[S, A] => IOSetter[S, A] = st => new IOSetter[S, A] {
          def modify(f: A => A): State[S, Unit] = State.modify(st.modify(f))
        }
      }
    }

    /* IOGetter */

    trait IOGetterAlg[A, P[_]] {
      def get: P[A]
    }

    type IOGetter[S, A] = IOCoalgebra[IOGetterAlg[A, ?[_]], Reader, S]

    object IOGetter {

      def getterIso[S, A] = new (Getter[S, A] <=> IOGetter[S, A]) {

        def from: IOGetter[S, A] => Getter[S, A] =
          iogt => Getter[S, A](iogt.get.run)

        def to: Getter[S, A] => IOGetter[S, A] = gt => new IOGetter[S, A] {
          def get: Reader[S, A] = Reader(gt.get)
        }
      }
    }
  }

  object OpticsAndStateConnections {
    import scalaz.{ Monad, MonadState }
    import OpticsAsCoalgebras.IOLens

    type MSLens[S, A] = MonadState[State[S, ?], A]

    def lensIso[S, A] = new (IOLens[S, A] <=> MSLens[S, A]) {

      def from: MSLens[S, A] => IOLens[S, A] = msln => new IOLens[S, A] {
        def get: State[S, A] = msln.get
        def set(a: A): State[S, Unit] = msln.put(a)
      }

      def to: IOLens[S, A] => MSLens[S, A] = ioln => new MSLens[S, A] {
        private val SM: Monad[State[S, ?]] = Monad[State[S, ?]]

        def point[A](a: => A): State[S, A] = SM.point(a)
        def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
          SM.bind(fa)(f)
        def get: State[S, A] = ioln.get
        def put(a: A): State[S, Unit] = ioln.set(a)
        def init: State[S, A] = get
      }
    }
  }

  object MonocleAndState {
    import Function.const
    import scalaz.syntax.monad._
    import monocle.macros.GenLens
    import monocle.state.all._
    import OpticsAsCoalgebras.IOLens

    case class Person(name: String, age: Int)
    val p: Person = Person("John", 30)

    /* Example using Monocle's state module */

    val _age: Lens[Person, Int] = GenLens[Person](_.age)
    val increment: State[Person, Int] = _age mod (_ + 1)
    increment.run(p) shouldEqual (Person("John", 31), 31)

    /* Example using IOLens (returns Unit instead) */

    val _ioage: IOLens[Person, Int] =
      IOLens[Person, Int](_.age)(age => _.copy(age = age))
    val ioincrement: State[Person, Int] =
      (_ioage modify (_ + 1)) >> (_ioage.get)
    ioincrement.run(p) shouldEqual (Person("John", 31), 31)
  }

  "IOLens" should "work" in MonocleAndState

  object DiscussionAndOngoingWork {
    import scalaz.IndexedState
    import monocle.PLens

    /* First approach: different P[_] and Q[_] */

    type IOCoalgebra[IOAlg[_[_], _[_]], Step[_, _, _], S, T] =
      IOAlg[Step[S, S, ?], Step[S, T, ?]]

    trait IOPLensAlg[A, B, P[_], Q[_]] {
      def get: P[A]
      def set(b: B): Q[Unit]
    }

    trait PBind[F[_], G[_]] {
      def pbind[A, B](fa: F[A])(f: A => G[B]): G[B]
    }

    object IOPLensAlg {
      import scalaz.{ Functor, Monad }
      import scalaz.Isomorphism.<=>
      import scalaz.syntax.monad._

      def gets[A, B, C, P[_], Q[_]](
          f: A => C)(implicit
          PQ: IOPLensAlg[A, B, P, Q],
          F: Functor[P]): P[C] =
        PQ.get map f

      def modify[A, B, P[_], Q[_]](
          f: A => B)(implicit
          PQ: IOPLensAlg[A, B, P, Q],
          PB: PBind[P, Q]): Q[Unit] =
        PB.pbind(PQ.get)(f andThen PQ.set)
    }

    type IOPLens[S, T, A, B] =
      IOCoalgebra[IOPLensAlg[A, B, ?[_], ?[_]], IndexedState, S, T]

    object IOPLens {

      def plensIso[S, T, A, B] = new (PLens[S, T, A, B] <=> IOPLens[S, T, A, B]) {

        def from: IOPLens[S, T, A, B] => PLens[S, T, A, B] =
          ioln => PLens(ioln.get.eval)(b => ioln.set(b).exec)

        def to: PLens[S, T, A, B] => IOPLens[S, T, A, B] =
          ln => new IOPLens[S, T, A, B] {
            def get: IndexedState[S, S, A] =
              IndexedState(s => (s, ln.get(s)))
            def set(b: B): scalaz.IndexedState[S, T, Unit] =
              IndexedState(s => (ln.set(b)(s), ()))
          }
      }
    }
  }
}
