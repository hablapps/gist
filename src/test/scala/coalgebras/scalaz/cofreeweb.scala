package org.hablapps.gist
package coalgebras
package scalazimpl

import akka.http.scaladsl._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling._
import akka.http.scaladsl.marshalling._
import akka.stream.Materializer

import scala.concurrent.Future
import scalaz.{StateT, ~>}
import scalaz.std.scalaFuture._

import CoChurchFinalCoalgebra.Cofree
import Automata.Input.ADT

object WebEntity {

  trait CofreeWeb[I, Y] {
    type X
    var x: X
    val f: X => Y
    val coalg: Automata[Future, I, X]

    val handler: HttpRequest => Future[HttpResponse]
  }

  trait MyTypeClass[I] {
    def fromEntity: FromEntityUnmarshaller[I]
  }

  def cofree[I](
      host: String,
      port: Int,
      httpExt: HttpExt)(implicit
      mat: Materializer,
      typ: MyTypeClass[I]) =
    new CofreeCoalgebra2[CofreeWeb[I, ?], Automata[Future, I, ?]] {
      import httpExt.system.dispatcher

      type YCat[Y] = ToEntityMarshaller[Y]

      def label[Y: YCat](cy: CofreeWeb[I, Y]): Y =
        cy.f(cy.x)

      def machine[Y]: Automata[Future, I, CofreeWeb[I, Y]] =
        new Automata[Future, I, CofreeWeb[I, Y]] {
          def isFinal(): StateT[Future, CofreeWeb[I, Y], Boolean] =
            StateT[Future, CofreeWeb[I, Y], Boolean] { cw =>
              cw.coalg
                .isFinal()
                .eval(cw.x)
                .map((cw, _))
            }
          def next(i: I): StateT[Future, CofreeWeb[I, Y], Unit] =
            StateT[Future, CofreeWeb[I, Y], Unit] { cw =>
              cw.coalg
                .next(i)
                .eval(cw.x)
                .map((cw, _))
            }
        }

      def trace[_X: Automata[Future, I, ?], Y: YCat](_f: _X => Y): _X => CofreeWeb[I, Y] =
        _x => new CofreeWeb[I, Y] {
          type X = _X

          var x: X = _x
          val coalg = implicitly[Automata[Future, I, X]]
          val handler: HttpRequest => Future[HttpResponse] = {
            case HttpRequest(HttpMethods.POST, Uri.Path("/next"), _, entity, _) =>
              typ.fromEntity(entity) flatMap { input =>
                coalg.next(input).run(x) map { case (s, o) =>
                  x = s
                  HttpResponse()
                }
              }
            case HttpRequest(HttpMethods.GET, Uri.Path("/isFinal"), _, _, _) =>
              coalg.isFinal().run(x) map { case (s, o) =>
                x = s
                HttpResponse(entity = HttpEntity(o.toString))
              }
            case HttpRequest(HttpMethods.GET, Uri.Path("/"), _, _, _) =>
              Marshal(f(x)).to[HttpResponse]
          }

          httpExt.bindAndHandleAsync(handler, host, port)

          val f: X => Y = _f
        }

    }
}

import akka.actor.ActorSystem
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.stream.ActorMaterializer
import spray.json._

object WebEntityTest extends App with DefaultJsonProtocol {
  import WebEntity._
  import AnAutomata._

  case class Wrapper[A](a: A)
  object Wrapper {
    implicit def jsonFormat[A: JsonFormat]: RootJsonFormat[Wrapper[A]] =
      jsonFormat1(Wrapper.apply[A])
  }

  implicit val system = ActorSystem("cofree-web")
  import system.dispatcher
  implicit val materializer = ActorMaterializer()
  implicit object foo extends MyTypeClass[Boolean] {
    def fromEntity: FromEntityUnmarshaller[Boolean] =
      implicitly[FromEntityUnmarshaller[Wrapper[Boolean]]].map(_.a)
  }
  val httpExt = Http()

  implicit val CofreeAutomata = cofree[Boolean]("localhost", 8080, httpExt)

  CofreeAutomata.trace[Int, String](_.toString)(Even[Future], implicitly[ToEntityMarshaller[String]])(0)

}