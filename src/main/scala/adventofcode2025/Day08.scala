package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day08:
  case class JunctionBox(x: Int, y: Int, z: Int)

  def euclidianDistance(j1: JunctionBox, j2: JunctionBox): Double =
    math.sqrt(
      math.pow(j1.x.toDouble - j2.x.toDouble, 2) +
        math.pow(j1.y.toDouble - j2.y.toDouble, 2) +
        math.pow(j1.z.toDouble - j2.z.toDouble, 2)
    )

  def connectBoxes(boxes: List[JunctionBox], connections: Int): Int =
    @tailrec
    def loop(pairsToConnect: List[List[JunctionBox]], circuits: List[List[JunctionBox]]): Int =
      pairsToConnect match
        case Nil                       =>
          circuits.map(_.size).sorted.takeRight(3).product
        case (box1 :: box2 :: _) :: tl =>
          val (toMergeCircuits, otherCircuits) = circuits
            .partition(circuit => circuit.contains(box1) || circuit.contains(box2))
          loop(tl, otherCircuits :+ toMergeCircuits.flatten)

    val pairsToConnect =
      boxes.combinations(2).toList.sortBy(pair => euclidianDistance(pair.head, pair.last)).take(connections)
    loop(pairsToConnect, boxes.map(box => List(box)))

  def connectBoxesIntoOneCircuit(boxes: List[JunctionBox]): Long =
    @tailrec
    def loop(
        pairsToConnect: List[List[JunctionBox]],
        circuits: List[List[JunctionBox]],
        lastPairToConnect: List[JunctionBox]
    ): Long =
      if circuits.length == 1 then
        lastPairToConnect.head.x.toLong * lastPairToConnect.last.x.toLong
      else
        val pairToConnect                    = pairsToConnect.head
        val (toMergeCircuits, otherCircuits) = circuits
          .partition(circuit => circuit.contains(pairToConnect.head) || circuit.contains(pairToConnect.last))
        loop(pairsToConnect.tail, otherCircuits :+ toMergeCircuits.flatten, pairToConnect)

    val pairsToConnect = boxes.combinations(2).toList.sortBy(pair => euclidianDistance(pair.head, pair.last))
    loop(pairsToConnect, boxes.map(box => List(box)), List.empty[JunctionBox])

  def parse(input: String): List[JunctionBox] =
    input.split('\n').toList.map:
      case s"$x,$y,$z" => JunctionBox(x.toInt, y.toInt, z.toInt)

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day08input.txt")): source =>
      source.mkString
end Day08
