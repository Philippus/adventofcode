package adventofcode2017

import org.apache.pekko.actor.PoisonPill
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using

object Day18:
  def followInstructions(instructions: Seq[String]): Long =
    @tailrec
    def loop(currentPos: Long, registers: Map[String, Long]): Long =
      if currentPos >= instructions.length then
        -1
      else
        instructions(currentPos.toInt) match
          case s"snd $a"                                                                 =>
            loop(currentPos + 1, registers + ("snd" -> registers.getOrElse(a, 0)))
          case s"set $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> b.toLong))
          case s"set $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> registers.getOrElse(b, 0)))
          case s"add $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) + b.toLong)))
          case s"add $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) + registers.getOrElse(b, 0L))))
          case s"mul $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) * b.toLong)))
          case s"mul $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) * registers.getOrElse(b, 0L))))
          case s"mod $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) % b.toLong)))
          case s"mod $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) % registers.getOrElse(b, 0L))))
          case s"rcv $a" if registers.getOrElse("snd", 0) == 0                           =>
            loop(currentPos + 1, registers)
          case s"rcv $a"                                                                 =>
            registers.getOrElse("snd", 0)
          case s"jgz 1 3"                                                                =>
            loop(currentPos + 3, registers)
          case s"jgz $a $b" if b.toLongOption.nonEmpty && registers.getOrElse(a, 0L) > 0 =>
            loop(currentPos + b.toLong, registers)
          case s"jgz $a $b" if b.toLongOption.isEmpty && registers.getOrElse(a, 0L) > 0  =>
            loop(currentPos + registers.getOrElse(b, 0L), registers)
          case s"jgz $a"                                                                 =>
            loop(currentPos + 1, registers)
    loop(0L, Map.empty)

  sealed trait Command
  final case class SetTarget(ref: ActorRef[Command]) extends Command
  case class Sup(ref: ActorRef[Any])                 extends Command
  final case class Enqueue(l: Long)                  extends Command
  case object Next                                   extends Command
  class Program(p: Int, instructions: Seq[String]):

    def apply(): Behavior[Command] = idle()

    private def idle(): Behavior[Command] = Behaviors.receive: (context, message) =>
      (context, message) match
        case (ctx, SetTarget(actorRef))       =>
          context.self ! Next
          active(actorRef, 0L, 0, Map("p" -> p), Vector.empty)
        case (_, Sup(replyTo: ActorRef[Any])) =>
          replyTo ! HelloWorldMain.State("idle", 0, p)
          Behaviors.same
        case _                                =>
          Behaviors.unhandled

    private def active(
        actorRef: ActorRef[Command],
        count: Long,
        currentPos: Int,
        registers: Map[String, Long],
        queue: Vector[Long]
    ): Behavior[Command] =
      Behaviors.receive: (context, message) =>
        (context, message) match
          case (ctx, Next)                      =>
            if currentPos >= instructions.length then
              finished(count)
            else
              instructions(currentPos) match
                case s"snd $a" if a.toLongOption.nonEmpty                                      =>
                  actorRef ! Enqueue(a.toLong)
                  context.self ! Next
                  active(actorRef, count + 1, currentPos + 1, registers, queue)
                case s"snd $a"                                                                 =>
                  actorRef ! Enqueue(registers.getOrElse(a, 0))
                  context.self ! Next
                  active(actorRef, count + 1, currentPos + 1, registers, queue)
                case s"rcv $a" if queue.nonEmpty                                               =>
                  context.self ! Next
                  active(actorRef, count, currentPos + 1, registers + (a -> queue.head), queue.tail)
                case s"rcv $a"                                                                 =>
                  waiting(actorRef, count, currentPos, registers, queue)
                case s"set $a $b" if b.toLongOption.nonEmpty                                   =>
                  context.self ! Next
                  active(actorRef, count, currentPos + 1, registers + (a -> b.toLong), queue)
                case s"set $a $b"                                                              =>
                  context.self ! Next
                  active(actorRef, count, currentPos + 1, registers + (a -> registers.getOrElse(b, 0)), queue)
                case s"add $a $b" if b.toLongOption.nonEmpty                                   =>
                  context.self ! Next
                  active(
                    actorRef,
                    count,
                    currentPos + 1,
                    registers + (a -> (registers.getOrElse(a, 0L) + b.toLong)),
                    queue
                  )
                case s"add $a $b"                                                              =>
                  context.self ! Next
                  active(
                    actorRef,
                    count,
                    currentPos + 1,
                    registers + (a -> (registers.getOrElse(a, 0L) + registers.getOrElse(b, 0L))),
                    queue
                  )
                case s"mul $a $b" if b.toLongOption.nonEmpty                                   =>
                  context.self ! Next
                  active(
                    actorRef,
                    count,
                    currentPos + 1,
                    registers + (a -> (registers.getOrElse(a, 0L) * b.toLong)),
                    queue
                  )
                case s"mul $a $b"                                                              =>
                  context.self ! Next
                  active(
                    actorRef,
                    count,
                    currentPos + 1,
                    registers + (a -> (registers.getOrElse(a, 0L) * registers.getOrElse(b, 0L))),
                    queue
                  )
                case s"mod $a $b" if b.toLongOption.nonEmpty                                   =>
                  context.self ! Next
                  active(
                    actorRef,
                    count,
                    currentPos + 1,
                    registers + (a -> (registers.getOrElse(a, 0L) % b.toLong)),
                    queue
                  )
                case s"mod $a $b"                                                              =>
                  context.self ! Next
                  active(
                    actorRef,
                    count,
                    currentPos + 1,
                    registers + (a -> (registers.getOrElse(a, 0L) % registers.getOrElse(b, 0L))),
                    queue
                  )
                case s"jgz 1 3"                                                                =>
                  context.self ! Next
                  if (currentPos + 3) > Int.MaxValue.toLong then
                    active(actorRef, count, Int.MaxValue, registers, queue)
                  else
                    active(actorRef, count, currentPos + 3, registers, queue)
                case s"jgz $a $b" if b.toLongOption.nonEmpty && registers.getOrElse(a, 0L) > 0 =>
                  context.self ! Next
                  active(actorRef, count, currentPos + b.toInt, registers, queue)
                case s"jgz $a $b" if b.toLongOption.isEmpty && registers.getOrElse(a, 0L) > 0  =>
                  context.self ! Next
                  active(actorRef, count, currentPos + registers.getOrElse(b, 0L).toInt, registers, queue)
                case s"jgz $a"                                                                 =>
                  context.self ! Next
                  active(actorRef, count, currentPos + 1, registers, queue)
          case (ctx, Enqueue(l: Long))          =>
            active(actorRef, count, currentPos, registers, queue :+ l)
          case (_, Sup(replyTo: ActorRef[Any])) =>
            replyTo ! HelloWorldMain.State("active", count, p)
            active(actorRef, count, currentPos, registers, queue)

    private def waiting(
        actorRef: ActorRef[Command],
        count: Long,
        currentPos: Int,
        registers: Map[String, Long],
        queue: Vector[Long]
    ): Behavior[Command] =
      Behaviors.receive: (context, message) =>
        (context, message) match
          case (ctx, Enqueue(l: Long))          =>
            ctx.self ! Next
            active(actorRef, count, currentPos, registers, queue :+ l)
          case (_, Sup(replyTo: ActorRef[Any])) =>
            replyTo ! HelloWorldMain.State("waiting", count, p)
            waiting(actorRef, count, currentPos, registers, queue)

    private def finished(count: Long): Behavior[Command] =
      Behaviors.receive: (context, message) =>
        (context, message) match
          case (_, Sup(replyTo: ActorRef[Any])) =>
            replyTo ! HelloWorldMain.State("finished", count, p)
            finished(count)
  end Program

  object HelloWorldMain:

    case object Boot
    case object Tick
    case class State(state: String, count: Long, program: Int)

    def apply(instructions: Seq[String]): Behavior[Any] =
      Behaviors.setup: context =>
        Behaviors.withTimers: timers =>
          val programZero = context.spawn(new Program(0, instructions)(), "zero")
          val programOne  = context.spawn(new Program(1, instructions)(), "one")

          val programsState                                  = mutable.Set.empty[(Int, String)]
          def updateState(program: Int, state: String): Unit =
            programsState.update((program, state), true)

          timers.startTimerAtFixedRate(Tick, 2.seconds)

          Behaviors.receive: (context, message) =>
            message match
              case Boot                                            =>
                programZero ! SetTarget(programOne)
                programOne ! SetTarget(programZero)
                Behaviors.same
              case Tick                                            =>
                programZero ! Sup(context.self)
                programOne ! Sup(context.self)
                Behaviors.same
              case State(state: String, count: Long, program: Int) =>
                updateState(program, state)
                println(s"program $program, state $state, count is $count")
                if programsState.forall(_._2 == "waiting") then
                  context.self ! PoisonPill
                  Behaviors.same
                else
                  Behaviors.same
  end HelloWorldMain

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2017/day18input.txt")):
      _.getLines().toSeq
end Day18
