package org.hablapps.gist

import org.scalatest._

class LambdaBasics extends FlatSpec with Matchers {

  /*************************************************************/
  /* 1. Non-Recursive datatypes: same Church & Scott Encodings */
  /*************************************************************/

  // 1.1. Booleans

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

    def and(b1: Bool, b2: Bool): Bool = b1(b2(True, False), False)

    def or(b1: Bool, b2: Bool): Bool = b1(True, b2(True, False))

    def negate(b: Bool): Bool = b(False, True)

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

  // 1.2. Tuples

  trait Tuple[B, C] {
    def apply[A](both: (B, C) => A): A
  }

  case class StringIntTuple(s: String, i: Int) extends Tuple[String, Int] {
    def apply[A](both: (String, Int) => A): A = both(s, i)
  }

  object Tuple {

    def fst[A, B](t: Tuple[A, B]): A = t((a, _) => a)

    def snd[A, B](t: Tuple[A, B]): B = t((_, b) => b)

    def swap[B, C](t: Tuple[B, C]): Tuple[C, B] = new Tuple[C, B] {
      def apply[A](both: (C, B) => A): A = t((b, c) => both(c, b))
    }
  }

  "Tuple expressions" should "work" in {
    import Tuple._

    val t = StringIntTuple("text", 0)

    fst(t) shouldBe "text"
    snd(t) shouldBe 0
    swap(t) ((i, s) => s"$i -> $s") shouldBe "0 -> text"
  }

  // 1.3. Temperatures

  trait Temperature {
    def apply[A](f: Int => A, c: Int => A): A
  }

  case class Fahrenheit(i: Int) extends Temperature {
    def apply[A](f: Int => A, c: Int => A): A = f(i)
  }

  case class Celsius(i: Int) extends Temperature {
    def apply[A](f: Int => A, c: Int => A): A = c(i)
  }

  object Temperature {
    def warm(t: Temperature): Boolean = t[Boolean](_ > 90, _ > 30)
  }

  "Temperature expressions" should "work" in {
    import Temperature._

    val t1: Temperature = Fahrenheit(40)
    val t2: Temperature = Celsius(40)

    warm(t1) shouldBe false
    warm(t2) shouldBe true
  }

  /**************************/
  /* 2. Recursive datatypes */
  /**************************/

  // 2.1. Naturals

  // 2.1.1. Scott Encoding

  trait SNat {
    def apply[A](zero: A, succ: SNat => A): A
  }

  case object SZero extends SNat {
    def apply[A](zero: A, succ: SNat => A): A = zero
  }

  case object SOne extends SNat {
    def apply[A](zero: A, succ: SNat => A): A = succ(SZero)
  }

  case class SSucc(n: SNat) extends SNat {
    def apply[A](zero: A, succ: SNat => A): A = succ(n)
  }

  object SNat {

    def add(n: SNat, m: SNat): SNat = n(m, add(_, m))

    // O(1) complexity
    def pred(n: SNat): SNat = n(SZero, identity)

    // aka. church to scott...
    def foldNat[A](zero: A, succ: A => A): SNat => A =
      _(zero, n => succ(foldNat(zero, succ)(n)))

    def xmatch[A](n: SNat)(z: A, s: SNat => A): A = n(z, s)

    object Zero {
      def unapply(n: SNat): Option[Unit] =
        xmatch[Option[Unit]](n)(Option(()), _ => Option.empty)
    }

    object Succ {
      def unapply(n: SNat): Option[SNat] =
        xmatch[Option[SNat]](n)(Option.empty, Option.apply)
    }

    def toChurch: SNat => CNat = foldNat(CZero, CSucc)
  }

  "Scott's Nat expressions" should "work" in {
    import SNat._

    pred(SOne)[Int](0, _ => 1) shouldBe 0
    pred(SSucc(SOne))[Int](0, _ => 1) shouldBe 1

    add(SZero, SZero)(0, _ => 1) shouldBe 0
    add(SZero, SOne)(0, _ => 1) shouldBe 1

    (SZero match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 0

    (SOne match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 2

    (SSucc(SOne) match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 1
  }

  // 2.1.2. Church Encoding

  trait CNat {
    def apply[A](zero: A, succ: A => A): A
  }

  object CZero extends CNat {
    def apply[A](zero: A, succ: A => A): A = zero
  }

  object COne extends CNat {
    def apply[A](zero: A, succ: A => A): A = succ(CZero[A](zero, succ))
  }

  case class CSucc(n: CNat) extends CNat {
    def apply[A](zero: A, succ: A => A): A = succ(n(zero, succ))
  }

  object CNat {
    import Bool.negate

    def add(n: CNat, m: CNat): CNat = n(m, CSucc)

    def mul(n: CNat, m: CNat): CNat = n[CNat](CZero, add(m, _))

    def exp(n: CNat, m: CNat): CNat = n[CNat](COne, mul(m, _))

    def isEven(n: CNat): Bool = n(True, negate)

    def isOdd(n: CNat): Bool = negate(isEven(n))

    // It takes `O(n)` to get the predecessor!
    def pred(n: CNat): CNat =
      n[(CNat, CNat)]((CZero, CZero), { case (_, x) => (x, CSucc(x)) })._1

    def xmatch[A](n: CNat)(z: A, s: CNat => A): A =
      n[A](z, _ => s(pred(n)))

    object Zero {
      def unapply(n: CNat): Option[Unit] =
        xmatch[Option[Unit]](n)(Option(()), _ => Option.empty)
    }

    object Succ {
      def unapply(n: CNat): Option[CNat] =
        xmatch[Option[CNat]](n)(Option.empty, Option.apply)
    }

    def toScott: CNat => SNat = _[SNat](SZero, SSucc)
  }

  "Church's Nat expressions" should "work" in {
    import CNat._

    isEven(CZero)       (true, false) shouldBe true
    isEven(COne)        (true, false) shouldBe false
    isEven(CSucc(COne)) (true, false) shouldBe true

    isOdd(CZero) (true, false) shouldBe false
    isOdd(COne)  (true, false) shouldBe true

    isEven(add(CZero, CZero)) (true, false) shouldBe true
    isEven(add(CZero, COne))  (true, false) shouldBe false
    isEven(add(COne,  CZero)) (true, false) shouldBe false
    isEven(add(COne,  COne))  (true, false) shouldBe true

    isEven(mul(CZero, CZero))             (true, false) shouldBe true
    isEven(mul(CZero, COne))              (true, false) shouldBe true
    isEven(mul(CSucc(COne), CSucc(COne))) (true, false) shouldBe true

    pred(CZero)[Int](0, _ + 1)              shouldBe 0 // XXX: weird behaviour!
    pred(COne)[Int](0, _ + 1)               shouldBe 0
    pred(CSucc(COne))[Int](0, _ + 1)        shouldBe 1
    pred(CSucc(CSucc(COne)))[Int](0, _ + 1) shouldBe 2

    (CZero match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 0

    (COne match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 2

    (CSucc(COne) match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 1
  }

  // 2.1.3. Parigot Encoding

  trait PNat {
    def apply[A](zero: A, succ: (PNat, A) => A): A
  }

  case object PZero extends PNat {
    def apply[A](zero: A, succ: (PNat, A) => A): A = zero
  }

  case object POne extends PNat {
    def apply[A](zero: A, succ: (PNat, A) => A): A = succ(PZero, zero)
  }

  case class PSucc(n: PNat) extends PNat {
    def apply[A](zero: A, succ: (PNat, A) => A): A = succ(n, n(zero, succ))
  }

  object PNat {

    def add(n: PNat, m: PNat): PNat = n[PNat](m, (p, a) => PSucc(a))

    def pred(n: PNat): PNat = n[PNat](PZero, (p, _) => p)

    def xmatch[A](n: PNat)(z: A, s: PNat => A): A = n[A](z, (p, _) => s(p))

    object Zero {
      def unapply(n: PNat): Option[Unit] =
        xmatch[Option[Unit]](n)(Option(()), _ => Option.empty)
    }

    object Succ {
      def unapply(n: PNat): Option[PNat] =
        xmatch[Option[PNat]](n)(Option.empty, Option.apply)
    }
  }

  "Parigot's Nat expressions" should "work" in {
    import PNat._

    pred(POne)[Int](0, (_, i) => i) shouldBe 0
    pred(PSucc(POne))[Int](0, (_, i) => i + 1) shouldBe 1

    add(PZero, PZero)[Int](0, (_, i) => i) shouldBe 0
    add(PZero, POne)[Int](0, (_, i) => i + 1) shouldBe 1
    add(POne, POne)[Int](0, (_, i) => i + 1) shouldBe 2

    (PZero match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 0

    (POne match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 2

    (PSucc(POne) match {
      case Zero(_)       => 0
      case Succ(Succ(_)) => 1
      case Succ(_)       => 2
    }) shouldBe 1
  }

  // 2.2. Lists

  // 2.2.1. Scott Encoding

  trait SIntList {
    def apply[A](nil: => A, cons: (Int, SIntList) => A): A
  }

  object SNil extends SIntList {
    def apply[A](nil: => A, cons: (Int, SIntList) => A): A = nil
  }

  case class SCons(h: Int, t: SIntList) extends SIntList {
    def apply[A](nil: => A, cons: (Int, SIntList) => A): A = cons(h, t)
  }

  object SIntList {

    // O(1) complexity
    def head(l: SIntList): Int =
      l(throw new Error("Nil.head"), (i, _) => i)

    // O(1) complexity
    def tail(l: SIntList): SIntList =
      l(throw new Error("Nil.tail"), (_, t) => t)

    def foldIntList[A](nil: A, cons: (Int, A) => A): SIntList => A =
      _(nil, (i, xs) => cons(i, foldIntList(nil, cons)(xs)))
  }

  "Scott's Lists expressions" should "work" in {
    import SIntList._

    head(SCons(1, SNil)) shouldBe 1
    head(SCons(2, SCons(1, SNil))) shouldBe 2
    head(tail(SCons(2, SCons(1, SNil)))) shouldBe 1
  }

  // 2.2.2. Church Encoding

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
    def concat(l1: IntList, l2: IntList): IntList = l1(l2, Cons)
  }

  "Church's Lists expressions" should "work" in {
    import IntList._

    concat(Nil, Nil).apply[Int](0, _ + _) shouldBe 0
    concat(Cons(1, Nil), Nil).apply[Int](0, _ + _) shouldBe 1
    concat(Cons(1, Nil), Cons(2, Nil)).apply[Int](0, _ + _) shouldBe 3
  }

  // 2.2.3. Parigot Encoding

  trait PIntList {
    def apply[A](nil: => A, cons: (Int, PIntList, => A) => A): A
  }

  case object PNil extends PIntList {
    def apply[A](nil: => A, cons: (Int, PIntList, => A) => A): A = nil
  }

  case class PCons(x: Int, xs: PIntList) extends PIntList {
    def apply[A](nil: => A, cons: (Int, PIntList, => A) => A): A =
      cons(x, xs, xs(nil, cons))
  }

  object PIntList {

    def head(l: PIntList): Int =
      l[Int](throw new Error("Nil.head"), (i, _, _) => i)

    def tail(l: PIntList): PIntList =
      l[PIntList](throw new Error("Nil.tail"), (_, p, _) => p)

    def foldIntList[A](nil: A, cons: (Int, A) => A): PIntList => A =
      _[A](nil, (i, _, a) => cons(i, a))
  }

  "Parigot's Lists expressions" should "work" in {
    import PIntList._

    head(PCons(1, PNil)) shouldBe 1
    head(PCons(2, PCons(1, PNil))) shouldBe 2
    head(tail(PCons(2, PCons(1, PNil)))) shouldBe 1
    foldIntList[Int](0, (i, a) => i + a)(PCons(1, PCons(2, PNil))) shouldBe 3
  }
}
