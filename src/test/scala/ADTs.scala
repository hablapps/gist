package org.hablapps.gist

import org.scalatest._

/*
This gist revolves around deep embeddings using a simple language for arithmetic 
expressions. Besides dealing with ADTs, we will also mention interpreters, both 
compositional and non-compositional ones. 

Algebras are not explicitly used anywhere in the code. For a comparison
with algebraic concepts, see this gist: ...
*/
class ADTs extends FlatSpec with Matchers{

  /*
  Let's start with a simple ADT (Algebraic Data Type) representation of expressions.
  As it's customary, ADTs in Scala are represented as `sealed abstract class`es.
  */
  sealed abstract class Expr
  case class Lit(i: Int) extends Expr
  case class Neg(e: Expr) extends Expr
  case class Add(e1: Expr, e2: Expr) extends Expr

  /*
  This kind of representation allows us to write expressions as follows
  */
  val e1: Expr = Add(Lit(1), Neg(Lit(2))) // 1 + (-2)
  val e2: Expr = Neg(Neg(Lit(2))) // (-(-2))
  val e3: Expr = Neg(Neg(Add(Lit(1), Neg(Add(Neg(Lit(1)),Lit(2))))))
  val e4: Expr = Add(Lit(1), Add(Lit(1),Neg(Lit(2))))

  /*
  In order to evaluate these expressions, converting them to strings, transforming
  them into normal forms, etc., we implement independent functions. These functions
  can be understood as different "interpreters" of the language of arithmetic expressions. 
  We can consider two kinds of interpreters: compositional and non-compositional.
  In order to understand the different between both types of interpreters, note that 
  arithmetic expressions have a recursive structure. If the result of some interpreter
  for a given expression can always be obtained from the result of the interpreter for
  its subexpressions, then the interpreter is said to be compositional. Otherwise, it's
  non-compositional.
  */

  trait CompositionalInterpreters{
    def eval(e: Expr): Int
    def write(e: Expr): String
  }

  trait NonCompositionalInterpreters{    
    def pushNeg(e: Expr): Expr
    def reassociate(e: Expr): Expr
  }

  /*
  And here there are some simple tests for the implemented functions
  */
  case class TestCompositional(interpreters: CompositionalInterpreters){
    import interpreters._

    write(e1) should be("(1+(-2))")
    write(e2) should be("(-(-2))")
    write(e3) should be("(-(-(1+(-((-1)+2)))))")
    write(e4) should be("(1+(1+(-2)))")

    eval(e1) should be(-1)
    eval(e3) should be(eval(e4))
  }

  case class TestNonCompositional(interpreters: NonCompositionalInterpreters){
    import interpreters._

    pushNeg(e1) should be(e1)
    pushNeg(e2) should be(Lit(2))
    pushNeg(e3) should be(e4)

    reassociate(Add(Add(Add(Lit(1),Lit(2)),Lit(3)),Lit(4))) should 
      be(Add(Lit(1), Add(Lit(2), Add(Lit(3), Lit(4)))))
  } 

  /*
  We can implement our interpreters, both compositional and non-compositional, using pattern
  matching.
  */
  object PatternMatchingInterpreters 
    extends NonCompositionalInterpreters 
    with CompositionalInterpreters{

    def eval(e: Expr): Int = e match {
      case Lit(i) => i
      case Neg(e) => -eval(e)
      case Add(e1,e2) => eval(e1) + eval(e2)
    }

    def write(e: Expr): String = e match{
      case Lit(i) => s"$i"
      case Neg(e) => s"(-${write(e)})"
      case Add(e1,e2) => s"(${write(e1)}+${write(e2)})"
    }

    def pushNeg(e: Expr): Expr = e match {
      case Lit(i) => e
      case Neg(Lit(_)) => e
      case Neg(Neg(e1)) => pushNeg(e1)
      case Neg(Add(e1,e2)) => Add(pushNeg(Neg(e1)), pushNeg(Neg(e2)))
      case Add(e1,e2) => Add(pushNeg(e1), pushNeg(e2))
    }

    def reassociate(e: Expr): Expr = e match {
      case Add(Add(e1,e2), e3) => reassociate(Add(e1, Add(e2,e3)))
      case Add(e1, e2) => Add(e1, reassociate(e2))
      case Neg(e1) => Neg(reassociate(e1))
      case _ => e
    }
  }
    
  "Pattern matching" should "work" in {
    TestCompositional(PatternMatchingInterpreters)
    TestNonCompositional(PatternMatchingInterpreters)
  }

  /*
  Or else, we can try implementing those interpreters using predefined recursion
  schemes. For instance, we can try using `fold`s.
  */

  def fold[A](lit: Int => A, neg: A => A, add: (A,A) => A): Expr => A = {
    case Lit(i) => lit(i)
    case Neg(e) => neg(fold(lit,neg,add)(e))
    case Add(e1,e2) => add(fold(lit,neg,add)(e1),fold(lit,neg,add)(e2))
  }

  /* 
  But with this kind of recursion scheme we can only implement (in a direct way)
  compositional interpreters.
  */
  object FoldInterpreters extends CompositionalInterpreters{
    
    def eval(e: Expr): Int = 
      fold[Int](i => i, -_, _ + _)(e)
      
    def write(e: Expr): String = 
      fold[String](
        i => s"$i", 
        e => s"(-$e)", 
        (e1,e2) => s"($e1+$e2)"
      )(e)
  }

  "Catamorphisms" should "work" in TestCompositional(FoldInterpreters)

}

object ADTs extends ADTs