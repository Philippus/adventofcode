package adventofcode2019

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day06:
  def orbits(objects: List[(String, String)]): Long =
    val map: mutable.Map[String, Long] = mutable.Map[String, Long]()

    while objects.exists(x => !map.contains(x._2)) do
      objects.foreach:
        case (a, b) if map.contains(a)                => map.update(b, map(a) + 1L)
        case (a, b) if !objects.map(_._2).contains(a) => map.update(a, 0L)
        case _                                        => ()
    map.values.sum

  def importLines(): List[(String, String)] =
    Using.resource(Source.fromResource("2019/day06input.txt")):
      _.getLines().toList.map(_.split(')')).flatMap:
        case Array(a, b) => List((a, b))
end Day06
