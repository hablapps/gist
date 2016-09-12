package org.hablapps.gist

import org.scalatest._

class ChurchBasics extends FlatSpec with Matchers {

  /* Church Encoding */

  // Booleans

  trait Bool {
    def apply[A](t: A, f: A): A
  }

  object True extends Bool {
    def apply[A](t: A, f: A): A = t
  }

  object False extends Bool {
    def apply[A](t: A, f: A): A = f
  }

  object Bool {

    def and(b1: Bool, b2: Bool): Bool = new Bool {
      def apply[A](t: A, f: A): A = b1(b2(t, f), f)
    }

    def or(b1: Bool, b2: Bool): Bool = new Bool {
      def apply[A](t: A, f: A): A = b1(t, b2(t, f))
    }

    def negate(b: Bool): Bool = new Bool {
      def apply[A](t: A, f: A): A = b(f, t)
    }

    def _if[A](b: Bool)(_then: A, _else: A): A = b(_then, _else)
  }

  "Bool expressions" should "work" in {
    import Bool._

    and(False, False) (true, false) shouldBe false
    and(True,  False) (true, false) shouldBe false
    and(False, True)  (true, false) shouldBe false
    and(True,  True)  (true, false) shouldBe true

    or(False, False) (true, false) shouldBe false
    or(True,  False) (true, false) shouldBe true
    or(False, True)  (true, false) shouldBe true
    or(True,  True)  (true, false) shouldBe true

    _if(True)  (_then = "hello", _else = "world") shouldBe "hello"
    _if(False) (_then = "hello", _else = "world") shouldBe "world"
  }

  // Naturals

  trait Nat {
    def apply[A](zero: A, succ: A => A): A
  }

  object Zero extends Nat {
    def apply[A](zero: A, succ: A => A): A = zero
  }

  object One extends Nat {
    def apply[A](zero: A, succ: A => A): A = succ(zero)
  }

  object Two extends Nat {
    def apply[A](zero: A, succ: A => A): A = succ(succ(zero))
  }

  object Nat {
    import Bool.negate

    def add(n1: Nat, n2: Nat): Nat = new Nat {
      def apply[A](zero: A, succ: A => A): A = n2(n1(zero, succ), succ)
    }

    def isEven(n: Nat): Bool = n(True, negate)

    def isOdd(n: Nat): Bool = negate(isEven(n))
  }

  "Nat expressions" should "work" in {
    import Nat._

    isEven(Zero) (true, false) shouldBe true
    isEven(One)  (true, false) shouldBe false
    isEven(Two)  (true, false) shouldBe true

    isOdd(Zero)  (true, false) shouldBe false
    isOdd(One)   (true, false) shouldBe true
    isOdd(Two)   (true, false) shouldBe false

    isEven(add(Zero, Zero)) (true, false) shouldBe true
    isEven(add(Zero, One))  (true, false) shouldBe false
    isEven(add(One,  Zero)) (true, false) shouldBe false
    isEven(add(One,  One))  (true, false) shouldBe true
    isEven(add(One,  Two))  (true, false) shouldBe false
    isEven(add(Two,  One))  (true, false) shouldBe false
    isEven(add(Two,  Two))  (true, false) shouldBe true
  }

  // Lists

  trait IntList {
    def apply[A](nil: A, cons: (Int, A) => A): A
  }

  object Nil extends IntList {
    def apply[A](nil: A, cons: (Int, A) => A): A = nil
  }

  case class Cons(h: Int, t: IntList) extends IntList {
    def apply[A](nil: A, cons: (Int, A) => A): A = cons(h, t(nil, cons))
  }

  object IntList {

    def concat(l1: IntList, l2: IntList): IntList = new IntList {
      def apply[A](nil: A, cons: (Int, A) => A): A = l1(l2(nil, cons), cons)
    }
  }
}
