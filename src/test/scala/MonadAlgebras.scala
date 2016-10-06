
object MonadAlgebras{

  trait TC_MonadAlgebra[P[_]]{
    def returns[A](a: A): P[A]
    def flatMap[A,B](fa: P[A])(f: A => P[B]): P[B]
  }

  sealed abstract class MonadF[P[_],_]
  case class Returns[P[_],A](a: A) extends MonadF[P,A]
  case class FlatMap[P[_],A,B](fa: P[A])(f: A => P[B]) extends MonadF[P,B]

  import scalaz.~>
  type F_MonadAlgebra[P[_]] = MonadF[P,?]~>P


}