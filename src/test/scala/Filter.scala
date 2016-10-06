package org.hablapps.gist

trait Filter[F[_]]{
  def filter[A](fa: F[A])(f: A => Boolean)(implicit 
    F: sourcecode.File, L: sourcecode.Line): F[A]
}

object Filter{

  def apply[F[_]](implicit S: Filter[F]) = S

  // Use in for-comprehensions

  implicit class FilterOps[F[_],A](fa: F[A])(implicit SF: Filter[F]){
    def filter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] = 
      SF.filter(fa)(f)
    def withFilter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] = 
      filter(f)
  }  

  import scalaz.MonadError, scalaz.syntax.monadError._

  type Location = (_root_.sourcecode.File,_root_.sourcecode.Line)

  def FilterForMonadError[F[_],S](error: Location => S)(
    implicit merror: MonadError[F,S]) = 
    new Filter[F]{
      def filter[A](fa: F[A])(f: A => Boolean)(implicit 
        F: sourcecode.File, L: sourcecode.Line): F[A] =
        merror.bind(fa)(a => 
          if (f(a)) a.point else merror.raiseError(error((F,L)))
        )
    }

  implicit def FilterForMonadErrorOnLocation[F[_]](implicit merror: MonadError[F,Location]) = 
    FilterForMonadError(identity)
}
