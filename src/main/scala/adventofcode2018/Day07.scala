package adventofcode2018

import scala.annotation.tailrec
import scala.collection.mutable
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

  def followInstructionsWithMultipleWorkers(steps: Seq[String], workers: Int, offset: Int): (String, Int) =
    val workerMap: mutable.Map[Int, Option[(Char, Int)]] = (for
      i <- 1 to workers
    yield i -> None).to(mutable.Map)

    var done = ""

    @tailrec
    def handlePt2(steps: Seq[String], tick: Int): (String, Int) =
      workerMap.foreach:
        case (i, None)                           => ()
        case (i, Some((c, ticks))) if ticks == 1 =>
          done += s"$c"
          workerMap.update(i, None)
        case (i, Some((c, ticks)))               =>
          workerMap.update(i, Some((c, ticks - 1)))
      val lefts: String  = steps.filterNot(step => done.contains(step.head)).map(_.head).mkString.distinct
      val rights: String = steps.filterNot(step => done.contains(step.head)).map(_.last).mkString.distinct
      val onlyAtRight    = steps.map(_.last).filterNot(c => steps.map(_.head).contains(c)).mkString.distinct
      var available      =
        lefts.distinct.diff(rights.distinct).filterNot(c => workerMap.exists(_._2.exists(_._1 == c))).sorted
      if available.isEmpty && workerMap.forall(_._2.isEmpty) then available = onlyAtRight
      if done.contains(onlyAtRight) && workerMap.forall(_._2.isEmpty) then
        (done, tick)
      else
        while available.nonEmpty && workerMap.exists(_._2.isEmpty) do
          val process = available.head
          workerMap.find(_._2.isEmpty).foreach: freeWorker =>
            workerMap.update(freeWorker._1, Some(available.head, available.head.toInt - 64 + offset))
          available = available.tail
        handlePt2(steps, tick + 1)

    handlePt2(steps, 0)

  def readSteps(steps: Seq[String]): Seq[String] =
    steps.map:
      case s"Step ${before} must be finished before step ${after} can begin." =>
        before ++ after

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2018/day07input.txt")):
      _.getLines().toSeq
end Day07
