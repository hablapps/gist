package org.hablapps.gist

import ObjectAlgebras.{IOAlg, EvalInstance}

import cats.{Monad, Eval}

/*
This gist shows how we can use and combine effects using object algebras (
IOAlg from the previous gist `ObjectAlgebras.scala` and FSAlg, an algebra to
build File System operations).

Actually we already saw how to combine algebras, as we treated Monad as a common algebra,
just like IO is. But for the sake of completeness we'll mix in another effect.

IMPORTANT: Compare this pattern with the one described on `FreeMonadCoproduct.scala`
*/
object ObjectAlgebrasMultipleEffects extends App {

  // As always we define our algebra to represent File System operations.
  trait FSAlg[F[_]] {
    def readFile(path: String): F[String]
    def deleteFile(path: String): F[Unit]
    def writeFile(path: String, content: String): F[Unit]
  }

  // Add the syntax
  object FSAlg {
    object syntax {
      def readFile[F[_]](path: String)(implicit FS: FSAlg[F]): F[String] =
        FS.readFile(path)
      def deleteFile[F[_]](path: String)(implicit FS: FSAlg[F]): F[Unit] =
        FS.deleteFile(path)
      def writeFile[F[_]](path: String)(content: String)(implicit FS: FSAlg[F]): F[Unit] =
        FS.writeFile(path, content)
    }
  }

  // Now we can import syntax from all the algebras involved and create our programs.
  object GenericProgramsMultipleEffects {
    import IOAlg.syntax._, FSAlg.syntax._, cats.syntax.flatMap._

    def copy[F[_]: IOAlg: FSAlg: Monad]: F[Unit] =
      write("Name a file you want to copy: ") >>
      read >>= { fileName =>
      readFile(fileName) >>=
      writeFile[F](s"$fileName copy")
      }
  }

  // We just need to add a concrete instance of our algebra, for `Eval` in this case.
  object EvalInstanceMultipleEffects {

    implicit object FSEval extends FSAlg[Eval] {
      import java.io.{File, FileWriter, BufferedWriter}
      def readFile(path: String): Eval[String] =
        Eval.always(scala.io.Source.fromFile(path).mkString)
      def deleteFile(path: String): Eval[Unit] = Eval.always {
        new File(path).delete()
        ()
      }
      def writeFile(path: String, content: String): Eval[Unit] = Eval.always {
        val bw = new BufferedWriter(new FileWriter(new File(path)))
        bw.write(content)
        bw.close()
      }
    }

    // And here we are, we can already instantiate the program, as we have instances of
    // our 3 algebras (IOAlg, FSAlg, Monad) for `Eval`.
    object Test {
      import GenericProgramsMultipleEffects._
      import EvalInstance.IOEval

      def apply() = copy[Eval].value

    }
  }

  EvalInstanceMultipleEffects.Test()

}
