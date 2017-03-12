This repository contains little (and not so little) snippets of code that illustrate techniques
from functional programming, mainly.

Current gists
=============

Some of these gists make reference or build upon results of previous ones. You'll also find
some explanations throughout the code, although, surely, not enough to make them self-contained. This is the list of current gists:

* [ADTs](src/test/scala/ADTs.scala). How do we represent embedded DSLs using algebraic data types, and how
do we implement both compositional and non-compositional interpreters.
* [GADTs](src/test/scala/GADTs.scala). How do we represent embedded DSLs using generalized algebraic data types, and how do we implement both compositional and non-compositional interpreters.
* [Church encodings](src/test/scala/ChurchEncodings.scala). What are Church encodings and how can we pattern match against them.
* [Church encodings HK](https://github.com/hablapps/gist/blob/hablacats/src/test/scala/ChurchEncodingsHK.scala). What are Higher-Kinded Church encodings and how can we pattern match against them.
* [Church vs. ADTs](src/test/scala/InitialAlgebras.scala). What is the relationship between these encodings? Algebras to the rescue!
* [Church vs. GADTs](https://github.com/hablapps/gist/blob/hablacats/src/test/scala/IsomorphismsHK.scala). Some isomorphisms between GADTs and Higher-Kinded Church encodings.
* [Natural Numbers](https://github.com/hablapps/gist/blob/hablacats/src/test/scala/NaturalEncodings.scala). Shows how we can represent Natural Numbers with different encodings: Church, Scott and Parigot.
* [Bypassing Free](src/test/scala/objectalgebras-vs-free-vs-eff). Check out how we can exploit object algebras to obtain the very same benefits of free monads, in many circumstances.
* [Coalgebras](src/test/scala/coalgebras). What are coalgebras? How are they related to monads and algebras in general? These gists attempt to shed some light to these questions.
* [Lens, State Is Your Father](src/test/scala/LensStateIsYourFather.scala). Encodings associated to a [blog post](https://blog.hablapps.com/2016/11/10/lens-state-is-your-father/). There, we provide `IOCoalgebra` representations for several optics, along with some interesting insights.
* [From "Hello, world!" to "Hello, monad!"](src/test/scala/hello-monads/). Code associated to the [blog post](https://blog.hablapps.com/2016/01/22/from-hello-world-to-hello-monad-part-i/) series about purification of effectful programs.
* [Macro `monad`](src/main/scala/MonadMacro.scala). A macro that allows you to write monadic code using neither `flatMap`s nor for-comprehensions, but conventional syntax, i.e. semicolons. Written with a pure didactic purpose: showing that monadic code is, in essence, simple imperative code.

Executing gists
===============

Each gist is implemented as a Scalatest file. In order to check its assertions, just enter `sbt` and launch the test. For instance, in order to launch the `ADTs` gist, enter `sbt` and type the following:

```scala
> test-only org.hablapps.gist.ADTs
```
