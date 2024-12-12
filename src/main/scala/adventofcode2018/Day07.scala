package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07:
  @tailrec
  def handle(available: String, done: String, steps: Seq[String]): String =
    if steps.isEmpty then done ++ available
    else
      val lefts: String  = steps.map(_.head).mkString
      val rights: String = steps.map(_.last).mkString
      val newAvailable   = available ++ lefts.distinct.diff(rights.distinct)
      val remainingSteps = steps.filterNot(_.head == newAvailable.min)
      handle(available.filterNot(char => char == newAvailable.min), done :+ newAvailable.min, remainingSteps)

  def bootstrap(steps: Seq[String]): String =
    val lefts: String  = steps.map(_.head).mkString
    val rights: String = steps.map(_.last).mkString
    val available      = lefts.distinct.diff(rights.distinct)
    val result         = handle(available, "", steps)
    result ++ rights.distinct.diff(result)

  def readSteps(steps: Seq[String]): Seq[String] =
    steps.map:
      case s"Step ${before} must be finished before step ${after} can begin." =>
        before ++ after

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2018/day07input.txt")):
      _.getLines().toSeq
end Day07
