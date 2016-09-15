package org.hablapps.gist

import org.scalatest._

class NaturalEncodings extends FlatSpec with Matchers {

  trait NatCases[S, E] {
    def zero: E
    def succ(s: S): E
  }

  object NatCases {

    def apply[S, E](z: E, f: S => E): NatCases[S, E] = new NatCases[S, E] {
      def zero: E = z
      def succ(s: S): E = f(s)
    }

    trait Syntax {
      def zero[S, E](implicit ev: NatCases[S, E]): E = ev.zero
      def succ[S, E](s: S)(implicit ev: NatCases[S, E]): E = ev.succ(s)
    }

    object syntax extends Syntax
  }

  type NatAlg[E] = NatCases[E, E]

  object NatAlg {
    def apply[E](z: E, f: E => E): NatAlg[E] = NatCases[E, E](z, f)
  }

  trait CNat {
    def apply[A](alg: NatAlg[A]): A
  }

  object CNat {

    // TODO: add fold evidences
    val initial: NatAlg[CNat] = NatAlg(
      new CNat {
        def apply[A](alg: NatAlg[A]): A = alg.zero
      },
      s => new CNat {
        def apply[A](alg: NatAlg[A]): A = alg.succ(s(alg))
      })

    val eval: NatAlg[Int] = NatAlg(0, _ + 1)

    object operator {
      def add(n: CNat, m: CNat): CNat = n(NatAlg(m, initial.succ))
    }
  }

  "Church's Nat algebra" should "work" in {
    import CNat._, initial._, operator._

    add(zero, zero)(eval) shouldBe 0
    add(zero, succ(zero))(eval) shouldBe 1
    add(succ(zero), succ(zero))(eval) shouldBe 2
  }

  trait SNat {
    def apply[A](alg: NatCases[SNat, A]): A
  }

  object SNat {

    val initial: NatAlg[SNat] = NatAlg(
      new SNat {
        def apply[A](alg: NatCases[SNat, A]): A = alg.zero
      },
      s => new SNat {
        def apply[A](alg: NatCases[SNat, A]): A = alg.succ(s)
      })

    val eval: NatCases[SNat, Int] = NatCases[SNat, Int](0, s => s(eval) + 1)

    object operator {
      def add(n: SNat, m: SNat): SNat =
        n(NatCases(m, p => initial.succ(add(p, m))))
    }
  }

  "Scott's Nat algebra" should "work" in {
    import SNat._, initial._, operator._

    add(zero, zero)(eval) shouldBe 0
    add(zero, succ(zero))(eval) shouldBe 1
    add(succ(zero), zero)(eval) shouldBe 1
    add(succ(zero), succ(zero))(eval) shouldBe 2
  }

  trait PNat {
    def apply[A](alg: NatCases[(PNat, A), A]): A
  }

  object PNat {

    val initial: NatAlg[PNat] = NatAlg(
      new PNat {
        def apply[A](alg: NatCases[(PNat, A), A]): A = alg.zero
      },
      s => new PNat {
        def apply[A](alg: NatCases[(PNat, A), A]): A =
          // XXX: `(x, y).map(f)` with cats, where are you?
          alg.succ(s match { case (x, y) => (x, y(alg)) })
      })

    // TODO: initial with NatAlg
    val qinitial = new NatCases[(PNat, PNat), PNat] {

      def zero: PNat = new PNat {
        def apply[A](alg: NatCases[(PNat, A), A]): A = alg.zero
      }

      def succ(s: (PNat, PNat)): PNat = new PNat {
        def apply[A](alg: NatCases[(PNat, A), A]): A =
          // XXX: `(x, y).map(f)` with cats, where are you?
          alg.succ(s match { case (x, y) => (x, y(alg)) })
      }
    }

    val eval: NatCases[(PNat, Int), Int] =
      NatCases[(PNat, Int), Int](0, { case (_, i) => i + 1 })

    object operator {
      def add(n: PNat, m: PNat): PNat =
        n(NatCases[(PNat, PNat), PNat](m, {
          case (p, a) => qinitial.succ(p, a)
        }))
    }
  }

  "Parigot's Nat algebra" should "work" in {
    import PNat._, qinitial._, operator._

    // add(zero, zero)(eval) shouldBe 0
    // add(zero, succ(zero))(eval) shouldBe 1
    // add(succ(zero), zero)(eval) shouldBe 1
    // add(succ(zero), succ(zero))(eval) shouldBe 2
  }
}
