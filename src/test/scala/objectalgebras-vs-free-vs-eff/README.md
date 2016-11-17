# Object algebras vs. Free vs. Eff

This gist aims to compare the common approach of using Free (Monads) to describe programs that are free of interpretation, with the not so known approach of using object algebras (type classes) to achieve the very same thing.

These two approaches are actually very related to each other:
* Given the *Functor* used with `Free` (`IOF[_]`) we can define an algebra `type IOAlg[F[_]] = IOF ~> F`
* That algebra is equivalent to the object algebra defined by the type class [`IOAlg[F[_]]`](ObjectAlgebras.scala#L19).
* `Free[IOF, ?]` is just an initial instance of that algebra. (It's an initial instance of the *Monad* algebra as well :wink:)
* Likewise, [`IO[A]`](ObjectAlgebras.scala#L41) is another initial instance of that algebra.

The Eff monad is also an example of a Free monad with a different approach for combining algebras (or "effects").

## Conclusion

In both cases we are working with initial algebras, and therefore we can achieve the very same functionality: namely, programming at the most abstract level, without committing to particular interpretations. That said, the use of object algebras is much more recommendable when we are dealing with compositional interpreters. In those cases, they have two major advantages over *Free*: there is no need to create instances of temporal classes (the Free ADT), and it's trivial to combine algebras and use the exact level of generality (e.g. to use Apply or Functor, instead of Monad). On the other hand, the use of *Free* is more recommendable when we need more control over the execution of our programs, since we have at our disposal the reification of our program.

### Related Gists

[Church vs. ADTs](https://github.com/hablapps/gist/blob/master/src/test/scala/InitialAlgebras.scala) What is the relationship between these encodings? Algebras to the rescue!

[ChurchEncodingsHK](https://github.com/hablapps/gist/blob/hablacats/src/test/scala/ChurchEncodingsHK.scala): We can also create non-compositional interpreters for object algebras, not easy though.
