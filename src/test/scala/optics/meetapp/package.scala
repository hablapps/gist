package org.hablapps.gist.optics

package object meetapp {

  case class Meetapp(
    counter: Int = 0,
    users: Map[Int, User] = Map.empty[Int, User],
    groups: Map[Int, Group] = Map.empty[Int, Group])

  case class User(name: String)

  case class Group(
    name: String,
    members: Map[Int, Member] = Map.empty[Int, Member])

  case class Member(uid: Int, bio: String)
}
