package adventofcode2022

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day11:
  case class Monkey(id: Int, op: String, divisibleBy: Int, ifTrue: Int, ifFalse: Int)

  def applyOp(operation: String, value: Long): Long =
    operation match
      case s"old * old" => value * value
      case s"old + old" => value + value
      case s"old * $b"  => value * b.toInt
      case s"old + $b"  => value + b.toInt
      case s"$a * old"  => a.toInt * value
      case s"$a + old"  => a.toInt + value
      case s"$a * $b"   => a.toInt * b.toInt
      case s"$a + $b"   => a.toInt + b.toInt

  def levelOfMonkeyBusiness(monkeyMap: Map[Monkey, Seq[Long]], part2: Boolean = false): Long =
    val mapWithInspections: mutable.Map[Monkey, (Seq[Long], Long)] =
      monkeyMap.map(a => a._1 -> (a._2, 0L)).to(mutable.Map)

    val productOfDivisibles = monkeyMap.keys.map(_.divisibleBy).product

    @tailrec
    def loop(round: Int): Long =
      if round > (if part2 then 10000 else 20) then
        mapWithInspections.values.map(_._2).toSeq.sorted.takeRight(2).product
      else
        mapWithInspections.keys.toSeq.sortBy(_.id).foreach: monkey =>
          mapWithInspections(monkey)._1.foreach: value =>
            val newWorryLevel  =
              if part2 then applyOp(monkey.op, value) % productOfDivisibles else applyOp(monkey.op, value) / 3
            val monkeyToUpdate = if newWorryLevel % monkey.divisibleBy == 0 then
              mapWithInspections.find(_._1.id == monkey.ifTrue).map(_._1).get
            else
              mapWithInspections.find(_._1.id == monkey.ifFalse).map(_._1).get
            mapWithInspections.update(
              monkeyToUpdate,
              (mapWithInspections(monkeyToUpdate)._1 :+ newWorryLevel, mapWithInspections(monkeyToUpdate)._2)
            )
            mapWithInspections.update(monkey, (mapWithInspections(monkey)._1.tail, mapWithInspections(monkey)._2 + 1))
          mapWithInspections.update(monkey, (Seq.empty[Long], mapWithInspections(monkey)._2))
        loop(round + 1)
    loop(1)

  def handleLines(lines: List[List[String]]): Map[Monkey, Seq[Long]] =
    lines.map:
      case s"Monkey $id:" :: s"Starting items: $items" :: s"Operation: new = $operation" :: s"Test: divisible by $divisibleBy" :: s"If true: throw to monkey $ifTrue" :: s"If false: throw to monkey $ifFalse" :: _ =>
        Monkey(id.toInt, operation, divisibleBy.toInt, ifTrue.toInt, ifFalse.toInt) -> items.split(", ").map(_.toLong).toSeq
    .toMap

  def importLines(): List[List[String]] =
    Using.resource(Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().map(_.trim).toList.grouped(7).toList
end Day11
