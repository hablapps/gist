package org.hablapps.gist

import org.scalatest._

class NaturalEncodings extends FlatSpec with Matchers {

  // Quasi Algebra for Naturals
  trait NatQAlg[S, E] {
    def zero: E
    def succ(s: S): E
  }

  object NatQAlg {

    def apply[S, E](z: E, f: S => E): NatQAlg[S, E] = new NatQAlg[S, E] {
      def zero: E = z
      def succ(s: S): E = f(s)
    }

    trait Syntax {
      def zero[S, E](implicit ev: NatQAlg[S, E]): E = ev.zero
      def succ[S, E](s: S)(implicit ev: NatQAlg[S, E]): E = ev.succ(s)
    }

    object syntax extends Syntax
  }

  type NatAlg[E] = NatQAlg[E, E]

  object NatAlg {
    def apply[E](z: E, f: E => E): NatAlg[E] = NatQAlg[E, E](z, f)
  }

  trait CNat {
    def apply[A](alg: NatAlg[A]): A
  }

  object CNat {

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
    def apply[A](alg: NatQAlg[SNat, A]): A
  }

  object SNat {

    val qinitial: NatQAlg[SNat, SNat] = new NatQAlg[SNat, SNat] {

      def zero: SNat = new SNat {
        def apply[A](alg: NatQAlg[SNat, A]): A = alg.zero
      }

      def succ(s: SNat): SNat = new SNat {
        def apply[A](alg: NatQAlg[SNat, A]): A = alg.succ(s)
      }
    }

    val eval: NatQAlg[SNat, Int] = NatQAlg[SNat, Int](0, s => s(eval) + 1)

    object operator {
      def add(n: SNat, m: SNat): SNat =
        n(NatQAlg(m, p => qinitial.succ(add(p, m))))
    }
  }

  "Scott's Nat algebra" should "work" in {
    import SNat._, qinitial._, operator._

    add(zero, zero)(eval) shouldBe 0
    add(zero, succ(zero))(eval) shouldBe 1
    add(succ(zero), zero)(eval) shouldBe 1
    add(succ(zero), succ(zero))(eval) shouldBe 2
  }

  trait PNat {
    def apply[A](alg: NatQAlg[(PNat, A), A]): A
  }

  object PNat {

    val qinitial = new NatQAlg[(PNat, PNat), PNat] {

      def zero: PNat = new PNat {
        def apply[A](alg: NatQAlg[(PNat, A), A]): A = alg.zero
      }

      def succ(s: (PNat, PNat)): PNat = new PNat {
        def apply[A](alg: NatQAlg[(PNat, A), A]): A =
          // XXX: `(x, y).map(f)` with cats, where are you?
          alg.succ(s match { case (x, y) => (x, y(alg)) })
      }
    }

    val eval: NatQAlg[(PNat, Int), Int] =
      NatQAlg[(PNat, Int), Int](0, { case (_, i) => i + 1 })

    object operator {
      def add(n: PNat, m: PNat): PNat =
        n(NatQAlg[(PNat, PNat), PNat](m, {
          case (p, a) => qinitial.succ(p, a)
        }))
    }
  }

  "Parigot's Nat algebra" should "work" in {
    import PNat._, qinitial._, operator._

    // These expressions make no sense, since `succ` requires a tuple as input
    // `(PNat, PNat)`

    // add(zero, zero)(eval) shouldBe 0
    // add(zero, succ(zero))(eval) shouldBe 1
    // add(succ(zero), zero)(eval) shouldBe 1
    // add(succ(zero), succ(zero))(eval) shouldBe 2

    // We "solve" this limitation replacing `succ` with the following method.
    def succ2(prev: PNat): PNat = succ((prev, prev))

    add(zero, zero)(eval) shouldBe 0
    add(zero, succ2(zero))(eval) shouldBe 1
    add(succ2(zero), zero)(eval) shouldBe 1
    add(succ2(zero), succ2(zero))(eval) shouldBe 2
    add(succ2(succ2(zero)), succ2(zero))(eval) shouldBe 3
  }

  // What about generic expressions?

  import NatQAlg.syntax._

  def zeroE[S, E](implicit ev: NatQAlg[S, E]): E = zero

  // XXX: Ops! zero's type is E, but `succ` is requiring S! In fact, what I was
  // trying to do was to force a Scott representation into a typeclass
  // expression (which we know it's almost Church encoding) That's why the ideal
  // of a generic expression is utopic.

  // def oneE[S, E](implicit ev: NatQAlg[S, E]): E = succ(zero)

  // => Well, it seems to be ¿impossible? to have a unique expression to be
  // shared by the different encodings. So what's the point of keeping `NatQAlg`
  // (disregarding the didactic comparision)?
}
