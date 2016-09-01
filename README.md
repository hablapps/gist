This repository contains little (and not so little) snippets of code that illustrate techniques
from functional programming, mainly.

Current gists
=============

Some of these gists make reference or build upon results of previous ones. You'll also find
some explanations throughout the code, although, surely, not enough to make them self-contained. This is the list of current gists:

* [ADTs](src/test/scala/ADTs.scala). How do we represent embedded DSLs using algebraic data types, and how
do we implement both compositional and non-compositional interpreters. 
* [Church encodings](src/test/scala/ChurchEncodings.scala). What are Church encodings and how can we pattern match against
them.
* [Church vs. ADTs](src/test/scala/InitialAlgebras.scala). What is the relationship between these encodings? Algebras to the
rescue!
* [GADTs](src/test/scala/GADTs.scala). How do we represent embedded DSLs using generalized algebraic data types, and how
do we implement both compositional and non-compositional interpreters.

Executing gists
===============

Each gist is implemented as a Scalatest file. In order to check its assertions, just enter `sbt` and launch the test. For instance, in order to launch the `ADTs` gist, enter `sbt` and type the following:

```scala
> test-only org.hablapps.gist.ADTs
```
