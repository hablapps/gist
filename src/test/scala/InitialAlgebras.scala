package org.hablapps.gist

/*
The purpose of this gist is explaining the relationships between ADT and Church
encodings using algebraic concepts. We'll use the familiar domain of arithmetic expressions.
Essentially, we'll see that given an algebraic theory for arithmetic expressions, ADT
and Church encodings correspond to initial algebras of that theory, completely equivalent
for all purposes (functionally speaking, since they may differ significantly in non-functional
concerns such as efficiency, modularity, etc.).

We will make reference to these others gists, on ADTs and Church encodings, respectively:
* https://github.com/hablapps/gist/blob/master/src/test/scala/ADTs.scala
* https://github.com/hablapps/gist/blob/master/src/test/scala/ChurchEncodings.scala

There are two basic ways of representing algebras in Scala: as functor algebras,
and as object algebras. We'll follow the later approach in this gist. For more information on
object algebras check the following source:

Extensibility for the Masses. Practical Extensibility with Object Algebras
Bruno C. d. S. Oliveira and William R. Cook
https://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf

*/

import org.scalatest._

class InitialAlgebras extends FlatSpec with Matchers{

  /*
  An algebraic theory is simply a collection of operations that allow us to build
  objects according to certain rules. In our case, we want to create arithmetic
  expressions.

  @jeslg: here, algebraic theory is synonim for typeclass or object algebra
  interface.

  The following trait tells us that we can create arithmetic expressions using the
  operations, or constructors, `lit`, `neg` and `add`. Any type `E` for which
  we can implement the following trait qualifies as an arithmetic expression.

  As you can see, object algebras are directly represented by type classes in Scala.
  */
  trait ExprAlg[E]{
    def lit(i: Int): E
    def neg(e: E): E
    def add(e1: E, e2: E): E
  }

  object ExprAlg{

    // Syntactical helper
    def apply[E](clit: Int => E, cneg: E => E, cadd: (E,E)=>E): ExprAlg[E] =
      new ExprAlg[E]{
        def lit(i: Int) = clit(i)
        def neg(e: E) = cneg(e)
        def add(e1: E, e2: E) = cadd(e1,e2)
      }

  }

  /*
  Which types can be regarded as arithmetic expressions? I.e. Which kinds of values
  can be created according to the rules of arithmetic expresions? There are many
  examples: integers, strings, etc. For instance:
  */

  object Eval{

    val algebra: ExprAlg[Int] =
      ExprAlg(i => i,
        i => -i,
        (i1,i2) => i1 + i2)

    /*
    Using the algebra, we can create integers as if they were aritmethic expressions
    */
    import algebra._

    add(lit(1),lit(2)) shouldBe 3
    add(neg(add(neg(lit(1)),lit(2))),lit(3)) shouldBe 2
  }

  object Write{

    val algebra: ExprAlg[String] =
      ExprAlg(i => i.toString,
        s => s"(-$s)",
        (s1,s2) => s"($s1+$s2)")

    /*
    This time, using the algebra we can create strings as if they were aritmethic expressions
    */
    import algebra._

    add(lit(1),lit(2)) shouldBe "(1+2)"
    add(neg(add(neg(lit(1)),lit(2))),lit(3)) shouldBe "((-((-1)+2))+3)"
  }

  "Sample algebras" should "work" in { Eval; Write }

  /*
  Although we have seen that strings and integers "are" arithmetic expressions, since there
  are expression algebras for them, it seems somewhat odd to say that. Intuitively,
  we may rather say that arithmetic expressions can be *interpreted* as strings or integers.
  From this perspective, expression algebras such as `Eval.algebra` and `Write.algebra` are
  the interpreters.

  Now, there are interpreters which are special, in the sense that they allow us to create
  arithmetic expressions that are fully general, and apparently free of any particular
  interpretation. For instance, let's consider an ADT representation of arithmetic expressions.
  We can indeed interpret the expression algebra over the `Expr` ADT.
  */

  object ADTAlgebra{

    // The constructors of the ADT are in a one-to-one correspondence with the
    // algebra operations
    import ADTs.{Lit,Neg,Add,Expr}

    val algebra: ExprAlg[Expr] = ExprAlg(Lit, Neg, Add)

    // Let's create some arithmetic expressions
    import algebra._

    add(lit(1),lit(2)) shouldBe Add(Lit(1),Lit(2))
    add(neg(add(neg(lit(1)),lit(2))),lit(3)) shouldBe Add(Neg(Add(Neg(Lit(1)),Lit(2))),Lit(3))

  }

  /*
  But not only we can understand ADT expressions through the lens of expression algebras.
  More importantly, we can obtain *any* other interpretation from it. Technically,
  this means that the ADT algebra is initial, i.e. there is an algebra homomorphism (and
  only one) from the ADT algebra to any other one. More informally, ADT expressions carry
  no particular meaning at all. So, given a particular interpretation (e.g. an algebra
  for integers), we can interpret the ADT expression to obtain a value in the domain of
  that interpretation. This is what the `fold` function accomplishes. Taking this into
  account, we can say that initial domains are canonical ways of representing algebraic
  expressions.

  In sum, an initial algebra is an algebra `Alg[I]` such that for any other possible
  algebra `Alg[A]`, we can interpret `I` as `A`:
  */

  trait InitialAlgebra[I, Alg[_]]{
    def algebra: Alg[I]
    def fold[A](alg: Alg[A]): I => A
  }

  /*
  We already implemented `fold` for the ADT representation, so we can easily check that
  ADT expressions make up an initial algebra.
  */
  object ADTInitial{
    import ADTs.Expr

    val initial = new InitialAlgebra[Expr, ExprAlg]{
      val algebra: ExprAlg[Expr] = ADTAlgebra.algebra

      def fold[A](alg: ExprAlg[A]): Expr => A =
        ADTs.fold(alg.lit, alg.neg, alg.add)
    }

    // We can now interpret ADT expressions as strings or integers using
    // the corresponding algebras (i.e. interpreters)
    import initial._, algebra._

    val e1: Expr = add(lit(1),lit(2)) // we create ADT expressions using
                                      // the "smart" constructors of the
                                      // algebra, rather than the ADT constructors.

    fold(Eval.algebra)(e1) shouldBe 3
    fold(Write.algebra)(e1) shouldBe "(1+2)"
  }

  "ADTInitial" should "work" in ADTInitial

  /*
  Besides ADT representations, in which other ways can we represent expressions in a
  canonical way? In other words, how can we construct other initial algebras? There are at
  least two other ways: one uses fixed points of functors, and the other one Church encodings.
  We will review this second one now.

  To motivate Church encodings, note that we have being using two particular expressions
  throughout this gist:

      add(lit(1),lit(2)), and
      add(neg(add(neg(lit(1)),lit(2))),lit(3))

  These expressions were written using the constructors `add`, `lit` and `neg` of particular
  expression algebras (`Eval.algebra`, `Write.algebra` or `ADTAlgebra.algebra`). Can we write
  these expressions just once, for any possible algebra? Yes, we can!
  */
  object TowardsChurch{

    // We simply build upon a generic algebra, instead of a particular one.
    def e0[E](alg: ExprAlg[E]): E = {
      import alg._
      add(lit(1),lit(2))
    }

    def e1[E](alg: ExprAlg[E]): E = {
      import alg._
      add(neg(add(neg(lit(1)),lit(2))),lit(3))
    }

    // Then, we can obtain the original interpretations by applying the corresponding
    // algebra to the generic expression

    import ADTs.{Lit, Add, Neg}

    e0(ADTAlgebra.algebra) shouldBe Add(Lit(1),Lit(2))
    e0(Write.algebra) shouldBe "(1+2)"
    e0(Eval.algebra) shouldBe 3
  }

  "Towards church" should "work" in TowardsChurch

  /*
  Now, please note the close similarity between the representation of arithmetic expressions
  using ad-hoc polymorphic functions such as `e0` and `e1`, and the Church encoding of
  arithmetic expressions:

  https://github.com/hablapps/gist/blob/master/src/test/scala/ChurchEncodings.scala#L45

  Essentially, the Church encoding is just a reification of a polymorphic function which
  creates an expression using a number of constructors. This polymorphic function is almost
  identical to the ones that we wrote before, `e0` and `e1`, the difference just being that
  the constructors in these later functions are packaged within an algebra.

  It's actually very easy to come up with the proof that Church encodings are initial
  algebras.
  */

  object ChurchInitial{
    import ChurchEncodings.Church, Church._

    val initial = new InitialAlgebra[Expr, ExprAlg]{

      val algebra: ExprAlg[Expr] = ExprAlg(Expr.lit, Expr.neg, Expr.add)

      def fold[A](alg: ExprAlg[A]): Expr => A =
        _(alg.lit, alg.neg, alg.add)
    }

    /*
    Using the Church algebra we can write the expressions `e0` and `e1` step-by-step,
    instead of performing a single instantiation (not saying that this is good, simply
    that you can).

    @jeslg: step-by-step means that `add`, `lit` and `neg` are creating
    intermediate expressions (reified by `Expr`)

    */

    import initial._, algebra._

    val e0: Expr = add(lit(1),lit(2))
    val e1: Expr = add(neg(add(neg(lit(1)),lit(2))),lit(3))

    /*
    Being a canonical domain, we can interpret Church expressions as integers, string,
    or even ADT expressions (another canonical domain), using their corresponding
    interpreters.
    */
    import ADTs.{Lit, Add, Neg}

    fold(ADTAlgebra.algebra)(e0) shouldBe Add(Lit(1),Lit(2))
    fold(Eval.algebra)(e0) shouldBe 3
    fold(Write.algebra)(e0) shouldBe "(1+2)"
  }

  "Church Initial" should "work" in ChurchInitial

}

object InitialAlgebras extends InitialAlgebras
