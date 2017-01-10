package org.hablapps.gist
package hello

// Hello, functional world!
object Step1 {

  /* Impure program */
  def hello(): Unit =
    println("Hello, world!")

  /* Functional purification */
  object Fun {

    // Language
    type IOProgram = Print
    case class Print(msg: String)

    // Program
    def pureHello(): IOProgram =
      Print("Hello, world!")

    // Interpreter
    def run(program: IOProgram): Unit =
      program match {
        case Print(msg) => println(msg)
      }

    // Composition
    def hello(): Unit = run(pureHello())
  }

}
