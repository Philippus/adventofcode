package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day04:
  case class Minute(guard_id: Int, asleepAt: Int)

  def applyStrategy(lines: Seq[String], strategy: Int = 1): Int =
    @tailrec
    def loop(lines: Seq[String], currentGuard: Int, asleepAt: Int, acc: Seq[Minute]): Int =
      if lines.isEmpty then
        if strategy == 1 then
          val mostAsleepGuard: Int = acc.groupBy(_.guard_id).toSeq.maxBy(_._2.length)._1
          val minute               = acc.filter(_.guard_id == mostAsleepGuard).groupBy(_.asleepAt).toSeq.maxBy(_._2.length)._1
          mostAsleepGuard * minute
        else
          val mostFrequentlyAsleep = acc.groupBy(x => (x.asleepAt, x.guard_id)).maxBy(_._2.length)
          mostFrequentlyAsleep._1._1 * mostFrequentlyAsleep._1._2
      else
        lines.head match
          case s"[$ignore] Guard #$guard_id begins shift" =>
            loop(lines.tail, guard_id.toInt, -1, acc)
          case s"[$ignore:$m] falls asleep"               =>
            loop(lines.tail, currentGuard, m.toInt, acc)
          case s"[$ignore:$m] wakes up"                   =>
            val minutesAsleep = (asleepAt until m.toInt).map(m => Minute(currentGuard, m))
            loop(lines.tail, currentGuard, -1, acc ++ minutesAsleep)

    loop(lines, -1, -1, Seq.empty)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source => source.getLines().toSeq.sorted
end Day04
