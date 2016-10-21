package org.hablapps.gist.optics.meetapp

import scalaz.Monad
import scalaz.State, State._
import scalaz.syntax.monad._
import scalaz.std.option._

import monocle._
import monocle.std.map._
import monocle.function.At, At._

object PureOptics {

  /* Optics */

  val _counter: Lens[Meetapp, Int] =
    Lens[Meetapp, Int](_.counter)(c => _.copy(counter = c))

  val _users: Lens[Meetapp, Map[Int, User]] =
    Lens[Meetapp, Map[Int, User]](_.users)(us => _.copy(users = us))

  def _user(uid: Int): Lens[Meetapp, Option[User]] =
    _users composeLens at[Map[Int, User], Int, Option[User]](uid)

  val _groups: Lens[Meetapp, Map[Int, Group]] =
    Lens[Meetapp, Map[Int, Group]](_.groups)(gs => _.copy(groups = gs))

  def _group(gid: Int): Lens[Meetapp, Option[Group]] =
    _groups composeLens at[Map[Int, Group], Int, Option[Group]](gid)

  def _members: Lens[Group, Map[Int, Member]] =
    Lens[Group, Map[Int, Member]](_.members)(ms => _.copy(members = ms))

  def _member(mid: Int): Lens[Group, Option[Member]] =
    _members composeLens at[Map[Int, Member], Int, Option[Member]](mid)

  def _memberGroup(gid: Int, mid: Int): Lens[Meetapp, Option[Member]] =
    flatCompose(_group(gid), _member(mid))

  def flatCompose[M[_]: Monad, A, B, C](
      ln1: Lens[A, M[B]],
      ln2: Lens[B, M[C]]): Lens[A, M[C]] =
    Lens[A, M[C]](
      a => ln1.get(a) >>= ln2.get)(
      c => a => ln1.set(ln1.get(a) map ln2.set(c))(a))

  /* Logic */

  def incrCounter: State[Meetapp, Int] =
    modify(_counter.modify(_ + 1)) >> gets(_counter.get)

  def addUser(user: User): State[Meetapp, Int] =
    for {
      uid <- incrCounter
      _   <- modify(_user(uid).set(Option(user)))
    } yield uid

  def removeUser(uid: Int): State[Meetapp, Unit] =
    modify(_user(uid).set(Option.empty))

  def addMember(gid: Int, member: Member): State[Meetapp, Int] =
    for {
      mid <- incrCounter
      _   <- modify(_memberGroup(gid, mid).set(Option(member)))
    } yield mid

  def removeMember(gid: Int, mid: Int): State[Meetapp, Unit] =
    modify(_memberGroup(gid, mid).set(Option.empty))

  def addGroup(group: Group, member: Member): State[Meetapp, (Int, Int)] =
    for {
      ids <- (incrCounter |@| incrCounter) { (_, _) }
      (gid, mid) = ids
      _ <- modify(_group(gid).set(Option(group)))
      _ <- modify(_memberGroup(gid, mid).set(Option(member)))
    } yield (gid, mid)

  import monocle.state.all._

  def removeGroup(gid: Int): State[Meetapp, Unit] =
    // modify(_group(gid).set(Option.empty))
    (_group(gid) assign Option.empty) >> (().point[State[Meetapp, ?]])

  /* Run it! */

  def myScenario: State[Meetapp, Unit] =
    for {
      jesus  <- addUser(User("jesus"))
      nids   <- addGroup(Group("ScalaMAD"), Member(jesus, "jesus@scala"))
      (scala, jesusScala) = nids
      javi   <- addUser(User("javi"))
      _      <- addMember(scala, Member(javi, "javi@scala"))
      juanma <- addUser(User("juanma"))
      nids   <- addGroup(Group("FP"), Member(juanma, "juanma@fp"))
      (fp, juanmaFp) = nids
      _      <- removeGroup(fp)
    } yield ()

  // println(myScenario.exec(Meetapp()))
}
