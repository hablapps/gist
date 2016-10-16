package org.hablapps.gist

import org.scalatest._

object Adhoc extends FlatSpec with Matchers {

  // Iso

  trait Iso[S, A] {

    def get(s: S): A

    def reverseGet(s: A): S
  }

  // Prism

  trait Prism[S, A] {

    def getOption(s: S): Option[A]

    def reverseGet(a: A): S
  }

  // Lens

  trait Lens[S, A] {

    def get(s: S): A

    def set(a: A): S => S
  }

  // Optional

  trait Optional[S, A] {

    def getOption(s: S): Option[A]

    def set(a: A): S => S
  }
}
