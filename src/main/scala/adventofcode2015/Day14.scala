package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day14:
  case class Reindeer(name: String, kps: Int, duration: Int, rest: Int)

  def traveled(reindeer: Reindeer, timelimit: Int): Int =
    val timesRan       = (timelimit.toDouble / (reindeer.duration + reindeer.rest).toDouble).floor.toInt
    val remainder      = timelimit - timesRan * (reindeer.duration + reindeer.rest)
    val ranInRemainder = scala.math.min(remainder, reindeer.duration)
    timesRan * reindeer.kps * reindeer.duration + (ranInRemainder * reindeer.kps)

  def raceReindeer(reindeer: Seq[Reindeer], timelimit: Int): Int =
    @tailrec
    def loop(scores: Set[(Reindeer, Int)], currentTime: Int): Int =
      if currentTime == timelimit then
        scores.map(score => score._2).max
      else
        val x                                              = reindeer.map(reindeer => (reindeer, traveled(reindeer, currentTime + 1)))
        val leadReindeer                                   = x.groupBy(_._2).toSeq.sortBy(-_._1).map(_._2).head.map(_._1)
        val newScoresForLeadReindeers: Seq[(Reindeer, Int)] = leadReindeer.map(leadReindeer =>
          scores
            .find(_._1 == leadReindeer)
            .map(score => (score._1, score._2 + 1)).get
        )
        loop(
          scores.filterNot(oldScore =>
            newScoresForLeadReindeers.exists(_._1 == oldScore._1)
          ) ++ newScoresForLeadReindeers,
          currentTime + 1
        )
    loop(reindeer.map(reindeer => (reindeer, 0)).toSet, 0)

  def handleLine(line: String): Reindeer =
    line match
      case s"$name can fly $kps km/s for $duration seconds, but then must rest for $rest seconds." =>
        Reindeer(name, kps.toInt, duration.toInt, rest.toInt)

  def distanceTraveledByWinningReindeer: Int =
    Using.resource(Source.fromResource("2015/day14input.txt")): source =>
      val reindeer = source.getLines().map(handleLine).toSeq
      reindeer.map(reindeer => traveled(reindeer, 2503)).max

  def scoreOfWinningReindeer: Int =
    Using.resource(Source.fromResource("2015/day14input.txt")): source =>
      val reindeer = source.getLines().map(handleLine).toSeq
      raceReindeer(reindeer, 2503)
end Day14
