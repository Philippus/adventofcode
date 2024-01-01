package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12:
  def findGroup(start: Int, map: Map[Int, Set[Int]]): Set[Int] =
    @tailrec
    def loop(acc: Set[Int]): Set[Int] =
      val newAcc = acc ++ acc.flatMap(key => map(key))
      if acc == newAcc then
        acc
      else loop(acc ++ newAcc)

    loop(Set(start))
  def countGroups(start: Int, map: Map[Int, Set[Int]]): Int    =
    def loop(acc: Set[Int], map: Map[Int, Set[Int]]): Int =
      val newAcc = acc ++ acc.flatMap(key => map(key))
      if acc == newAcc then
        val newMap = map.removedAll(acc)
        if newMap.isEmpty then
          1
        else
          1 + loop(Set(newMap.head._1), newMap)
      else loop(acc ++ newAcc, map)
    loop(Set(start), map)

  def handleLine(line: String): (Int, Set[Int]) =
    line match
      case s"$source <-> $dests" =>
        (source.toInt, dests.split(", ").map(_.toInt).toSet)

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2017/day12input.txt")):
      _.getLines().toSeq
end Day12
