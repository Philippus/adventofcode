package adventofcode2020

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day16:
  case class Rule(id: String, range: (Int, Int), orRange: (Int, Int))

  case class Ticket(values: Vector[Int])

  def errorScanningRate(rules: Vector[Rule], tickets: Vector[Ticket]): Int =
    def isValid(value: Int) =
      rules.exists: rule =>
        (rule.range._1 <= value && value <= rule.range._2) || (rule.orRange._1 <= value && value <= rule.orRange._2)

    val allValues = tickets.flatMap(_.values)
    allValues.filterNot(isValid).sum

  def multiplyDepartureFields(rules: Vector[Rule], yourTicket: Ticket, tickets: Vector[Ticket]): Long =
    def isValid(value: Int) =
      rules.exists: rule =>
        (rule.range._1 <= value && value <= rule.range._2) || (rule.orRange._1 <= value && value <= rule.orRange._2)

    val validTickets = tickets.filter:
      t => t.values.forall(isValid)

    val foundMap: mutable.Map[String, Int] = mutable.Map.empty[String, Int]
    while rules.filterNot(rule => foundMap.keySet.contains(rule.id)).exists(_.id.startsWith("departure")) do
      val fieldCandidates =
        for
          i <- yourTicket.values.indices
          if !foundMap.values.toVector.contains(i)
          valuesByIdx = (validTickets :+ yourTicket).map(t => t.values(i))
        yield
          rules.filterNot(rule => foundMap.keySet.contains(rule.id)).collect { case rule if
            valuesByIdx.forall: value =>
              (rule.range._1 <= value && value <= rule.range._2) || (rule.orRange._1 <= value && value <= rule.orRange._2)
          => (rule, i)
          }
      val foundCandidate = fieldCandidates.filter(_.length == 1).head.head
      foundMap.update(foundCandidate._1.id, foundCandidate._2)
    foundMap.filter(_._1.startsWith("departure")).map: f =>
      yourTicket.values(f._2).toLong
    .product

  def handleLines(lines: Vector[String]): (Vector[Rule], Ticket, Vector[Ticket]) =
    val rules      = lines.takeWhile(_.nonEmpty).map:
      case s"$id: $a-$b or $c-$d" =>
        Rule(id, (a.toInt, b.toInt), (c.toInt, d.toInt))
    val yourTicket =
      Ticket(lines.slice(lines.indexOf("your ticket:") + 1, lines.indexOf("your ticket:") + 2).head.split(',').map(
        _.toInt
      ).toVector)

    val nearbyTickets: Vector[Ticket] = lines.slice(lines.indexOf("nearby tickets:") + 1, lines.length).map(
      _.split(',').map(_.toInt).toVector
    ).map(Ticket(_))

    (rules, yourTicket, nearbyTickets)

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day16
