package org.atnos

import org.atnos.eff._
import all._
import org.atnos.eff.syntax.all._
import cats.data._
import cats.implicits._

object Main {

  // type alias for indicating the possibility of 
  // creating IO instructions in a stack of effects R
  type _io[R] = IOInst |= R

  // IO Instructions
  sealed abstract class IOInst[_]
  case class Read() extends IOInst[String]
  case class Write(msg: String) extends IOInst[Unit]

  // IO Programs
  def read[R :_io]: Eff[R, String] =
    send[IOInst, R, String](Read())

  def write[R :_io](msg: String): Eff[R, Unit] =
    send[IOInst, R, Unit](Write(msg))

  // A particular IO Program

  object SingleEffectProgram {
    def echo[R :_io]: Eff[R, String] = for {
      msg <- read
      _   <- write(msg)
    } yield msg
  }

  // Interpretation of IO programs

  import cats._

  // natural transformation from IO instructions to
  // Eval effects
  def consoleIO[R, U :_eval]: IOInst ~> Eff[U, ?] =
    new (IOInst ~> Eff[U, ?]) {
      import scala.io.StdIn
      def apply[T](inst: IOInst[T]): Eff[U, T] = inst match {
        case Read()     => delay[U, T](StdIn.readLine)
        case Write(msg) => delay[U, T](println(msg))
      }
  }

  // natural transformation from IO instructions to
  // State effects
  case class IOState(in: List[String], out: List[String]) {
    def addIn(i: String): IOState = copy(in = i :: in)
    def addOut(o: String): IOState = copy(out = o :: out)
  }

  type StateIO[A] = State[IOState, A]
  type _state[R] = StateIO |= R

  def stateAction[R, U :_state]: IOInst ~> Eff[U, ?] =
    new (IOInst ~> Eff[U, ?]) {
      def apply[T](inst: IOInst[T]): Eff[U, T] = inst match {
        case Read() => get[U, IOState].flatMap {
          case IOState(msg :: reads, writes) =>
            put[U, IOState](IOState(reads, writes)).as(msg)

          case other =>
            pure[U, T]("nothing to read from!")
        }

        case Write(msg) => get[U, IOState].flatMap {
          case IOState(reads, writes) =>
            put[U, IOState](IOState(reads, msg :: writes))
        }
      }
    }

  // Echo interpretations
  object SingleEffectInterpretations {
    import SingleEffectProgram._

    def consoleEcho[R, U](implicit m: Member.Aux[IOInst, R, U],
                                   eval: Eval |= U): Eff[U, String] =
      interpret.translateNat(echo[R])(consoleIO)

    def ioToState[R, U, A](e: Eff[R, A])(implicit m: Member.Aux[IOInst, R, U],
                                           state: StateIO |= U): Eff[U, A] =
      interpret.translateNat(e)(stateAction)

    type S = Fx.fx2[IOInst, StateIO]

    ioToState(echo[S]).evalState(IOState(List("hi"),List())).run == "hi"

    ioToState(echo[S]).execState(IOState(List("hi"),List())).run == IOState(List(),List("hi"))
  }

  // Logging instructions

  trait LogInst[A]
  case class Logging(level: Level, msg: String) extends LogInst[Unit]

  type _log[R] = LogInst |= R

  sealed abstract class Level
  case object WARNING extends Level
  case object DEBUG extends Level
  case object INFO extends Level

  // Log Programs
  def log[R :_log](level: Level, msg: String): Eff[R, Unit] =
    send[LogInst, R, Unit](Logging(level, msg))

  // Interpretations over IO actions
  def logAction[R, U :_io]: LogInst ~> Eff[U, ?] =
     new (LogInst ~> Eff[U, ?]) {
       def apply[T](t: LogInst[T]) =
         t match { case Logging(level, msg) => write(s"$level: $msg") }
     }

  def logToIo[R, U, A](e: Eff[R, A])(implicit m: Member.Aux[LogInst, R, U],
                                              io: IOInst |= U): Eff[U, A] =
    interpret.translateNat(e)(logAction)

  // Particular program

  object MultipleEffectProgram {

    def echo[R :_io :_log]: Eff[R, String] = for {
      msg <- read
      _   <- log(INFO, s"read '$msg'")
      _   <- write(msg)
      _   <- log(INFO, s"written '$msg'")
    } yield msg

  }

  // IO with logging programs work with io actions
  object MultipleEffectProgramTest {
    import MultipleEffectProgram._
    import SingleEffectInterpretations._

    val init: IOState = IOState(List("hi"),List())

    type S = Fx.fx3[LogInst, StateIO, IOInst]

    ioToState(logToIo(echo[S])).evalState(init).run == "hi"
    ioToState(logToIo(echo[S])).execState(init).run == IOState(List(), List("INFO: written 'hi'", "hi", "INFO: read 'hi'"))
  }

}
