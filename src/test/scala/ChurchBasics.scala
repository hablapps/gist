package org.hablapps.gist

import org.scalatest._

class ChurchBasics extends FlatSpec with Matchers {

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

    _if(True)(_then = "hello", _else = "world") shouldBe "hello"
    _if(False)(_then = "hello", _else = "world") shouldBe "world"
  }
}
