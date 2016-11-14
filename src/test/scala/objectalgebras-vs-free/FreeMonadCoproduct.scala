package org.hablapps.gist

import FreeMonad.{IOF, Read, Write, EvalInstance}

import cats.{Eval, ~>}
import cats.data.Coproduct
import cats.free.{Free, Inject}

/*
This gist shows how we can combine effects (IOF from the previous gist
`FreeMonad.scala` and FSF) using Coproduct and Free to successfully build
programs that are interpretation free.

IMPORTANT: Compare this pattern with the one described on
`ObjectAlgebrasMultipleEffects.scala`
*/
object FreeMonadCoproduct extends App {

  // As always we define our `Functor`, representing File System operations.
  sealed abstract class FSF[_]
  case class ReadFile(path: String) extends FSF[String]
  case class DeleteFile(path: String) extends FSF[Unit]
  case class WriteFile(path: String, content: String) extends FSF[Unit]

  // This time though, our "smart constructors" are going to be quite
  // different as we need to inject instructions from the functors (IOF
  // and FSF) to a certain functor that includes both of them. This
  // certain functor will be a `Coproduct` of all the functors involved,
  // but we don't need to be concrete at this point, so we leave it generic.
  //
  // Given a `Inject[IOF, F]`, we can lift instructions from `IOF[A]` to
  // `Free[F, A]`.
  object Coproducts {

    type FSInj[F[_]] = Inject[FSF, F]
    object FSInj {
      object syntax {
        def readFile[F[_]: FSInj](path: String): Free[F, String] =
          Free.inject[FSF, F](ReadFile(path))
        def deleteFile[F[_]: FSInj](path: String): Free[F, Unit] =
          Free.inject[FSF, F](DeleteFile(path))
        def writeFile[F[_]: FSInj](path: String)(content: String): Free[F, Unit] =
          Free.inject[FSF, F](WriteFile(path, content))
      }
    }

    // In opossition to object algebras, here we need new "smart constructors"
    // for IOF. The ones defined on `FreeMonad.scala` are no longer useful.
    type IOInj[F[_]] = Inject[IOF, F]
    object IOInj {
      object syntax {
        def read[F[_]: IOInj]: Free[F, String] =
          Free.inject[IOF, F](Read)
        def write[F[_]: IOInj](msg: String): Free[F, Unit] =
          Free.inject[IOF, F](Write(msg))
      }
    }
  }

  // Now we can import syntax from all the algebras involved and create our programs.
  object GenericProgramsWithCoproduct {
    import cats.syntax.flatMap._
    import Coproducts.IOInj, IOInj.syntax._
    import Coproducts.FSInj, FSInj.syntax._

    def copy[F[_]: IOInj: FSInj]: Free[F, Unit] =
      write("Name a file you want to copy: ") >>
      read >>= { fileName =>
      readFile(fileName) >>=
      writeFile(s"$fileName copy")
      }
  }

  // We just need to add a new interpreter for `Eval` in this case.
  object EvalInstanceWithCoproduct {

    object FSEval extends ~>[FSF, Eval] {
      import java.io.{File, FileWriter, BufferedWriter}
      def apply[A](fa: FSF[A]): Eval[A] = fa match {
        case ReadFile(path) => Eval.always(scala.io.Source.fromFile(path).mkString)
        case DeleteFile(path) => Eval.always {
            new File(path).delete()
            ()
          }
        case WriteFile(path, content) => Eval.always {
            val bw = new BufferedWriter(new FileWriter(new File(path)))
            bw.write(content)
            bw.close()
          }
      }
    }

    // We are very close to get it, what's left to do is:
    // - Concrete the `F[_]` to the coproduct of our effects (App[_])
    // - Combine both interpreters
    // - Run it!
    object Test {
      import GenericProgramsWithCoproduct._

      type App[A] = Coproduct[IOF, FSF, A]
      val interpreter: App ~> Eval = EvalInstance.IOEval or FSEval

      def apply() =
        copy[App].foldMap(interpreter).value

    }

  }

  EvalInstanceWithCoproduct.Test()

}
