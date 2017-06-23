package org.hablapps.gist

import org.scalatest._

class AbstractingEitherT extends FunSpec with Matchers{

  /** 
   * APIs for my business logic
   */
  
  // My business logic is gonna be imperative
  import cats.Monad

  // These are my domain-dependent instructions
  trait MyAPI[P[_]]{
    def bar(): P[Int]
  }

  object MyAPI{
    def apply[P[_]](implicit M: MyAPI[P]) = M
  }

  // Some errors returned by my business logic

  case class NegativeNumber(i: Int) extends Throwable

  /** 
   * Business logic implemented with EitherT and sample interpretation
   */

  object WithEitherT{
    import cats.syntax.applicative._, cats.syntax.flatMap._

    // Business logic 
    object BusinessLogic{
      import cats.data.EitherT

      def foo[P[_]: MyAPI: Monad](i: Int): EitherT[P,Throwable,String] = 
        EitherT.liftT[P,Throwable,Int](MyAPI[P].bar()) >>= { j => 
          if ((j-i)>=0) EitherT.right("ok".pure[P])
          else EitherT.left((NegativeNumber(j-i): Throwable).pure[P])
        }
    }
    
    object Interpretations{
      
      // Id interpretations
      import cats.Id

      implicit val IdMyAPI = new MyAPI[Id]{
        def bar(): Int = 10
      }
    }
  }

  describe("Foo with EitherT"){
    import cats.Id
    import WithEitherT._, BusinessLogic.foo, Interpretations._

    it("should work when everything is ok"){
      foo[Id](3).value shouldBe Right("ok")
    }

    it("should fail when necessary"){
      foo[Id](11).value shouldBe Left(NegativeNumber(-1))
    }
  }

  /** Business logic implemented with MonadError and smaple interpretation */

  object WithMonadError{
    import cats.MonadError

    object BusinessLogic{
      
      def foo[P[_]](i: Int)(implicit 
          M: MonadError[P,Throwable],
          My: MyAPI[P]): P[String] = 
        M.flatMap(My.bar()){ j => 
          if ((j-i)>=0) M.pure("ok")
          else M.raiseError(NegativeNumber(j-i))
        }

      // In this case syntax doesn't help a lot ... 
      import cats.syntax.applicative._, cats.syntax.applicativeError._, cats.syntax.flatMap._

      def fooWithSyntax[P[_]: MonadError[?[_],Throwable]: MyAPI](i: Int): P[String] = 
        MyAPI[P].bar() >>= { j => 
          if ((j-i)>=0) "ok".pure[P]
          else (NegativeNumber(j-i): Throwable).raiseError[P,String]
        }
    }
    
    object Interpretations{
      import cats.data.EitherT, cats.Id
    
      type MyProgram[T] = EitherT[Id,Throwable,T]
      
      // MyAPI instance for EitherT.
      implicit val EitherTMyAPI = new MyAPI[MyProgram]{
        def bar() = EitherT.right[Id,Throwable,Int](10)
      }
    }
  }

  describe("Foo with MonadError"){
    import WithMonadError._, BusinessLogic._, Interpretations._

    // Exactly the same tests as before ...

    it("should work when everything is ok"){
      foo[MyProgram](3).value shouldBe Right("ok")
    }

    it("should fail when necessary"){
      foo[MyProgram](11).value shouldBe Left(NegativeNumber(-1))
    }
  }


}