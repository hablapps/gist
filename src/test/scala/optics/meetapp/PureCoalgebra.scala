package org.hablapps.gist.optics
package meetapp

import scalaz.Monad
import scalaz.syntax.monad._

import IOCoalgebra._

object PureCoalgebras extends App {

  trait MeetappAlg[P[_]] {

    def incrCounter: P[Int]

    def addUser(user: User): P[Int]

    def removeUser(uid: Int): P[Unit]

    def addMember(gid: Int, member: Member): P[Int]

    def removeMember(gid: Int, mid: Int): P[Unit]

    def addGroup(group: Group, member: Member): P[(Int, Int)]

    def removeGroup(gid: Int): P[Unit]
  }

  trait MeetappMachine[Step[_, _], S] extends MeetappAlg[Step[S, ?]] {

    /* Algebras */

    implicit val _M: Monad[Step[S, ?]]

    val _counter: LensAlg[Int, Step[S, ?]]

    def _user(uid: Int): LensAlg[Option[User], Step[S, ?]]

    def _group(gid: Int): LensAlg[Option[Group], Step[S, ?]]

    def _memberGroup(gid: Int, mid: Int): LensAlg[Option[Member], Step[S, ?]]

    /* Logic */

    def incrCounter: Step[S, Int] =
      _counter.modify(_ + 1) >> _counter.get

    def addUser(user: User): Step[S, Int] =
      for {
        uid <- incrCounter
        _   <- _user(uid).set(Option(user))
      } yield uid

    def removeUser(uid: Int): Step[S, Unit] =
      _user(uid).set(Option.empty)

    def addMember(gid: Int, member: Member): Step[S, Int] =
      for {
        mid <- incrCounter
        _   <- _memberGroup(gid, mid).set(Option(member))
      } yield mid

    def removeMember(gid: Int, mid: Int): Step[S, Unit] =
      _memberGroup(gid, mid).set(Option.empty)

    def addGroup(group: Group, member: Member): Step[S, (Int, Int)] =
      for {
        ids <- (incrCounter |@| incrCounter) { (_, _) }
        (gid, mid) = ids
        _ <- _group(gid).set(Option(group))
        _ <- _memberGroup(gid, mid).set(Option(member))
      } yield (gid, mid)

    def removeGroup(gid: Int): Step[S, Unit] =
      _group(gid).set(Option.empty)
  }

  object MeetappMachine {

    def apply[Step[_, _], S](
        counter: LensAlg[Int, Step[S, ?]],
        user: Int => LensAlg[Option[User], Step[S, ?]],
        group: Int => LensAlg[Option[Group], Step[S, ?]],
        memberGroup: Int => Int => LensAlg[Option[Member], Step[S, ?]])(implicit
        M: Monad[Step[S, ?]]): MeetappMachine[Step, S] =
      new MeetappMachine[Step, S] {
        implicit val _M = M
        val _counter = counter
        def _user(uid: Int) = user(uid)
        def _group(gid: Int) = group(gid)
        def _memberGroup(gid: Int, mid: Int) = memberGroup(gid)(mid)
      }
  }

  /* Run it! */

  import scalaz.State

  def liftAlg[S, A](ln: monocle.Lens[S, A]): LensAlg[A, State[S, ?]] =
    new LensAlg[A, State[S, ?]] {
      val M: Monad[State[S, ?]] = Monad[State[S, ?]]
      def get = State.gets(ln.get)
      def set(a: A) = State.modify(ln.set(a))
    }

  implicit val StateMeetapp: MeetappAlg[State[Meetapp, ?]] =
    MeetappMachine[State, Meetapp](
      liftAlg(PureOptics._counter),
      uid => liftAlg(PureOptics._user(uid)),
      gid => liftAlg(PureOptics._group(gid)),
      gid => uid => liftAlg(PureOptics._memberGroup(gid, uid)))

  def myScenario[P[_]](implicit M: Monad[P], P: MeetappAlg[P]): P[Unit] =
    for {
      jesus  <- P.addUser(User("jesus"))
      nids   <- P.addGroup(Group("ScalaMAD"), Member(jesus, "jesus@scala"))
      (scala, jesusScala) = nids
      javi   <- P.addUser(User("javi"))
      _      <- P.addMember(scala, Member(javi, "javi@scala"))
      juanma <- P.addUser(User("juanma"))
      nids   <- P.addGroup(Group("FP"), Member(juanma, "juanma@fp"))
      (fp, juanmaFp) = nids
      _      <- P.removeGroup(fp)
    } yield ()

  println(myScenario[State[Meetapp, ?]].exec(Meetapp()))
}
