package org.hablapps.gist

import org.scalatest._

class NaturalEncodings extends FlatSpec with Matchers {

  trait InitialAlgebra[I, Alg[_]] {
    def algebra: Alg[I]
    def fold[A](alg: Alg[A]): I => A
  }

  trait NatCases[S, E] {
    def zero: E
    def succ(s: S): E
  }

  object NatCases {
    def apply[S, E](z: E, f: S => E): NatCases[S, E] = new NatCases[S, E] {
      def zero: E = z
      def succ(s: S): E = f(s)
    }
  }

  type NatAlg[E] = NatCases[E, E]

  object NatAlg {
    def apply[E](z: E, f: E => E): NatAlg[E] = NatCases[E, E](z, f)
  }

  trait CNat {
    def apply[A](alg: NatAlg[A]): A
  }

  object CNat {

    val initial = new InitialAlgebra[CNat, NatAlg] {

      val algebra: NatAlg[CNat] = NatAlg(
        new CNat {
          def apply[A](alg: NatAlg[A]): A = alg.zero
        },
        s => new CNat {
          def apply[A](alg: NatAlg[A]): A = alg.succ(s(alg))
        })

      def fold[A](alg: NatAlg[A]): CNat => A = _(alg)
    }

    val eval: NatAlg[Int] = NatAlg(0, _ + 1)

    def add(n: CNat, m: CNat): CNat = n(NatAlg(m, initial.algebra.succ))
  }

  "Church's Nat algebra" should "work" in {
    import CNat._, initial._, algebra._

    fold(eval)(zero) shouldBe 0
    fold(eval)(succ(zero)) shouldBe 1
    fold(eval)(succ(succ(zero))) shouldBe 2

    fold(eval)(add(zero, zero)) shouldBe 0
    fold(eval)(add(zero, succ(zero))) shouldBe 1
    fold(eval)(add(succ(zero), zero)) shouldBe 1
    fold(eval)(add(succ(zero), succ(zero))) shouldBe 2
  }

  trait SNat {
    def apply[A](alg: NatCases[SNat, A]): A
  }

  object SNat {

    val initial = new InitialAlgebra[SNat, NatAlg] {

      val algebra: NatAlg[SNat] = NatAlg(
        new SNat {
          def apply[A](alg: NatCases[SNat, A]): A = alg.zero
        },
        s => new SNat {
          def apply[A](alg: NatCases[SNat, A]): A = alg.succ(s)
        })

      def fold[A](alg: NatAlg[A]): SNat => A =
        _(NatCases[SNat, A](alg.zero, p => alg.succ(fold(alg)(p))))
    }

    def add(n: SNat, m: SNat): SNat =
      n(NatCases(m, p => initial.algebra.succ(add(p, m))))
  }

  "Scott's Nat algebra" should "work" in {
    import SNat._, initial._, algebra._
    import CNat.eval

    fold(eval)(zero) shouldBe 0
    fold(eval)(succ(zero)) shouldBe 1
    fold(eval)(succ(succ(zero))) shouldBe 2

    fold(eval)(add(zero, zero)) shouldBe 0
    fold(eval)(add(zero, succ(zero))) shouldBe 1
    fold(eval)(add(succ(zero), zero)) shouldBe 1
    fold(eval)(add(succ(zero), succ(zero))) shouldBe 2
  }

  trait PNat {
    def apply[A](alg: NatCases[(PNat, A), A]): A
  }

  object PNat {

    val initial = new InitialAlgebra[PNat, NatAlg] {

      val algebra: NatAlg[PNat] = NatAlg(
        new PNat {
          def apply[A](alg: NatCases[(PNat, A), A]): A = alg.zero
        },
        s => new PNat {
          def apply[A](alg: NatCases[(PNat, A), A]): A = alg.succ((s, s(alg)))
        })

      def fold[A](alg: NatAlg[A]): PNat => A =
        _(NatCases[(PNat, A), A](alg.zero, { case (_, a) => alg.succ(a) }))
    }

    def add(n: PNat, m: PNat): PNat =
      n(NatCases[(PNat, PNat), PNat](m, {
        case (_, a) => initial.algebra.succ(a)
      }))
  }

  "Parigot's Nat algebra" should "work" in {
    import PNat._, initial._, algebra._
    import CNat.eval

    fold(eval)(zero) shouldBe 0
    fold(eval)(succ(zero)) shouldBe 1
    fold(eval)(succ(succ(zero))) shouldBe 2

    fold(eval)(add(zero, zero)) shouldBe 0
    fold(eval)(add(zero, succ(zero))) shouldBe 1
    fold(eval)(add(succ(zero), zero)) shouldBe 1
    fold(eval)(add(succ(zero), succ(zero))) shouldBe 2
  }
}
