package org.hablapps.gist
package coalgebras
package scalazimpl

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scalaz.{Const, StateT}
import scalaz.std.scalaFuture._

import Automata.Input.{Next, IsFinal}

object ActorEntity {

  trait Proof
  object Proof {
    implicit val instance: Proof = new Proof {}
  }

  case object Label
  case class LabelResponse[Y](y: Y)

  // ACTOR

  class AutomataActor[I: ClassTag, X, Y](
    machine: Automata[Future, I, X],
    initialState: X,
    f: X => Y) extends Actor {

    import context.dispatcher

    var state: Future[X] = Future.successful(initialState)

    def receive = {
      case Next(i: I) =>
        val step = state flatMap machine.next(i).run
        state = step.map(_._1)
        step.map(_._2) pipeTo sender
      case IsFinal() =>
        val step = state flatMap machine.isFinal().run
        state = step.map(_._1)
        step.map(_._2) pipeTo sender
      case Label =>
        state map (f andThen LabelResponse.apply) pipeTo sender
    }
  }

  object AutomataActor {
    def props[I: ClassTag, X, Y](
      machine: Automata[Future, I, X],
      initialState: X,
      f: X => Y) = Props(new AutomataActor(machine, initialState, f))
  }

  // COFREE COALGEBRA FOR ENTITIES

  type CofreeActor[Y] = Const[ActorRef,Y]

  def cofree[I: ClassTag](
      as: ActorSystem)(implicit 
      timeout: Timeout) =
    new CofreeCoalgebra2[CofreeActor, Automata[Future, I, ?]] {
      import as.dispatcher

      type YCat[Y] = Proof

      def label[Y: YCat](cx: Const[ActorRef, Y]): Y =
        Await.result((cx.getConst ? Label).mapTo[LabelResponse[Y]].map(_.y), 5 seconds)

      def machine[Y]: Automata[Future, I, Const[ActorRef, Y]] =
        new Automata[Future, I, Const[ActorRef, Y]] {
          def isFinal(): StateT[Future, Const[ActorRef, Y], Boolean] =
            StateT { case c@Const(actor) =>
              (actor ? IsFinal())
                .mapTo[Boolean]
                .map((c, _))
            }
          def next(i: I): StateT[Future, Const[ActorRef, Y], Unit] =
            StateT { case c@Const(actor) =>
              (actor ? Next(i))
                .mapTo[Unit]
                .map((c, _))
            }
        }

      def trace[X: Automata[Future, I, ?], Y: YCat](
          f: X => Y): X => Const[ActorRef, Y] =
        x => Const[ActorRef, Y] {
          as.actorOf(AutomataActor.props[I, X, Y](implicitly[Automata[Future, I, X]], x, f))
        }
    }

}

object ActorEntityTest extends App {
  import ActorEntity._
  import AnAutomata._

  implicit val system = ActorSystem("cofree-actor")
  import system.dispatcher
  implicit val timeout = Timeout(5 seconds)

  val CofreeAutomata = cofree[Boolean](system)

  val Const(even) = CofreeAutomata.trace[Int, String](_.toString)(Even[Future], implicitly)(0)

  system.actorOf(Props(new Actor with ActorLogging {

    even ! IsFinal()
    even ! IsFinal()
    even ! IsFinal()
    even ! Next(true)
    even ! IsFinal()
    even ! IsFinal()
    even ! IsFinal()
    even ! Next(false)
    even ! IsFinal()
    even ! IsFinal()
    even ! IsFinal()

    def receive = {
      case x => log.info(s"Received: $x")
    }

  }))
}
