package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day23:
  def interconnectedComputers(
      computers: List[String],
      connections: List[Set[String]],
      amountInSet: Int
  ): List[Set[String]] =
    (for
      computer <- computers
      connected = connections.filter(_.contains(computer)).flatMap(_.toSeq).distinct
      combo    <- connected.combinations(amountInSet)
      if combo.combinations(2).forall:
        case List(a, b) => connections.contains(Set(a, b))
    yield combo).map(_.toSet).distinct

  def sumInterconnectedComputersWithT(computers: List[String], connections: List[Set[String]]): Int =
    interconnectedComputers(computers, connections, 3).count(_.exists(_.startsWith("t")))

  def maxPossibleComputersInSet(computers: List[String], connections: List[Set[String]]): Int =
    computers
      .map(computer => connections.filter(_.contains(computer)).flatMap(_.toSeq).distinct)
      .map(_.length)
      .max

  def findPasswordForLANParty(computers: List[String], connections: List[Set[String]]): String =
    var result      = ""
    var amountInSet = maxPossibleComputersInSet(computers, connections)
    while result.isEmpty do
      interconnectedComputers(computers, connections, amountInSet) match
        case l if l.nonEmpty => result = l.head.toSeq.sorted.mkString(",")
        case _               => ()
      amountInSet -= 1
    result

  def handleLines(lines: List[String]): (List[String], List[Set[String]]) =
    val computers   = lines.flatMap:
      case s"$c1-$c2" => List(c1, c2)
    val connections = lines.map:
      case s"$c1-$c2" => Set(c1, c2)
    (computers.distinct, connections)

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day23
