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
* [Bypassing Free](src/test/scala/objectalgebras-vs-free). Check out how we can exploit object algebras to obtain the very same benefits of free monads, in many circumstances. 
* [Coalgebras](src/test/scala/coalgebras). What are coalgebras? How are they related to monads and algebras in general? These gists attempt to shed some light to these questions.

Executing gists
===============

Each gist is implemented as a Scalatest file. In order to check its assertions, just enter `sbt` and launch the test. For instance, in order to launch the `ADTs` gist, enter `sbt` and type the following:

```scala
> test-only org.hablapps.gist.ADTs
```
