You'll find here an implementation of coalgebraic-related type classes and demonstrators. It's the accompanied code to the [Cádiz typelevel summit](http://typelevel.org/event/2016-09-conf-cadiz/)'s proposal "We are reative! Programming actor systems through cofree coalgebras". Some slides can be found [here](https://docs.google.com/presentation/d/16kBjlXNtPFnNjZCx2n4ZoPypPO5eUSt1cjAI7QxnVyU/edit?usp=sharing).

The `coalgebras` object package contains common definitions used in the different gists: `FinalCoalgebra`, `CofreeCoalgebra`, `F-Coalgebra`, `IO-Coalgebra`, etc. The `cats` and `scalaz` subpackages contains the following gists (note: `cats` version under preparation).

### Machines

* `automata.scala` ([scalaz](scalaz/automata.scala),cats), sample definition of a moore automata as an IO-coalgebra, i.e. as an interpretation of an input algebra over a state-based language; it's used throughout the other gists.
* `automatasample.scala` ([scalaz](scalaz/automatasample.scala),cats), sample instantiation of the moore automata.

### Programming machines

Given the IO language of the automata, we can implement different kinds of programs over the automata. The IO language just provides the "instructions" or buttons of the machine, which can be combined as we wish: monadically, applicatively, etc.

* `programmingimperatively.scala` ([scalaz](scalaz/programmingimperatively.scala),cats). Example of imperative programming over Moore automata.
* `programmingapplicatively.scala` ([scalaz](scalaz/programmingapplicatively.scala),cats). Sometimes, monadic combinators are not really needed. In that case, we can simply use applicative ones.
* `programmingwithexceptions.scala` ([scalaz](scalaz/programmingwithexceptions.scala),cats). We can also use pattern matching in for-comprehensions, with the help of the `MonadError` API.

### Universal machines

These are final and cofree coalgebra instantiations of Moore machines. 

* `finaladhoc.scala` ([scalaz](scalaz/finaladhoc.scala),cats), represents the behaviour of Moore automata in terms of the accepted language.
* `cofreecochurch.scala` ([scalaz](scalaz/cofreecochurch.scala),cats), represents the behaviour implicitly, in terms of the Church's dual encoding of greatest fix points.
* `cofreecomonad.scala` ([scalaz](scalaz/cofreecomonad.scala),cats), uses the cofree comonad for F-algebras.
* `cofreeactor.scala` ([scalaz](scalaz/cofreeactor.scala),cats), uses actors to provide a framework for execution in terms of cofree coalgebras. 
* `cofreeweb.scala` ([scalaz](scalaz/cofreeweb.scala),cats), does the same for a Web-based interface.

