package org.hablapps.gist

package object coalgebras{

  /* Algebras */

  import scalaz.~>
  type HK_F_Algebra[F[_[_], _], G[_]] = F[G,?]~>G 

  /* Coalgebras */

  type F_Coalgebra[F[_],S]=
    S=>F[S]

  /* IO Coalgebras */

  object IO{
  
    type Coalgebra[IOAlg[_[_]],Step[_,_],S]=
      IOAlg[Step[S,?]]

    import scalaz.StateT
  
    type CoalgebraFromFAlgebra[F[_[_],_],Step[_,_],S]=
      Coalgebra[HK_F_Algebra[F,?[_]], Step, S]
  }

  import scalaz.StateT
  type Entity[F[_],InputADT[_],S] = 
    HK_F_Algebra[λ[(_[_],T) => InputADT[T]], StateT[F,S,?]]
  // More simply: InputADT~>StateT[F,S,?]
  
  /* Final coalgebras */

  trait FinalCoalgebra[Final, Coalg[_]]{
    def coalgebra: Coalg[Final]
    def unfold[X](coalg: Coalg[X]): X => Final
  }

  object FinalCoalgebra{
    object Syntax{
      implicit class Unfold[Coalg[_],X](coalg: Coalg[X]){
        def unfold[Fi](implicit Fi: FinalCoalgebra[Fi,Coalg]) =
          Fi.unfold(coalg)
      }
    }

    implicit def toCoalg[Coalg[_],Fi](implicit Fi: FinalCoalgebra[Fi,Coalg]): Coalg[Fi] = 
      Fi.coalgebra
  }

  /* Cofree coalgebras */


  // We need this version of the cofree coalgebra type class for
  // the Web-based universal machine
  trait CofreeCoalgebra2[Cofree[_], Coalg[_]] {

    type YCat[_]

    def machine[Y]: Coalg[Cofree[Y]]
    def label[Y: YCat](cy: Cofree[Y]): Y
    def trace[X: Coalg, Y: YCat](f: X => Y): X => Cofree[Y]

    def trace[X: Coalg: YCat]: X => Cofree[X] =
      trace[X,X](identity[X])

  }

  trait CofreeCoalgebra[Cofree[_], Coalg[_]]{

    def machine[Y]: Coalg[Cofree[Y]]
    def label[X](cx: Cofree[X]): X
    def trace[X: Coalg, Y](f: X => Y): X => Cofree[Y]
    
    def trace[X: Coalg]: X => Cofree[X] = 
      trace[X,X](identity[X])
  }

  object CofreeCoalgebra{
    import scalaz.Comonad

    implicit def Comonad[Cofree[_],Coalg[_]](implicit C: CofreeCoalgebra[Cofree,Coalg]) = 
      new Comonad[Cofree]{
        def copoint[X](cx: Cofree[X]): X = 
          C.label(cx)

        def cobind[X, Y](cx: Cofree[X])(f: Cofree[X] => Y): Cofree[Y] =
          C.trace(f)(C.machine[X])(cx)

        def map[X, Y](cx: Cofree[X])(f: X => Y): Cofree[Y] =
          C.trace{ cx2: Cofree[X] => f(C.label(cx2)) }(C.machine[X])(cx)  
      }

    implicit def toFinalCoalgebra[Cofree[_],Coalg[_]](implicit C: CofreeCoalgebra[Cofree,Coalg]) = 
      new FinalCoalgebra[Cofree[Unit],Coalg]{
        def coalgebra = C.machine[Unit]
        def unfold[X](coalg: Coalg[X]) = C.trace{_ : X => ()}(coalg)
      }

    object Syntax{
      implicit class CofreeCoalgebraOps[Cofree[_],Coalg[_],Y](
        cf: Cofree[Y])(implicit C: CofreeCoalgebra[Cofree,Coalg]){
        def label(): Y = C.label(cf)
      }
    }
  }

}