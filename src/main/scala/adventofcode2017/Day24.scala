package adventofcode2017

import scala.io.Source
import scala.util.Using

object Day24:
  case class Component(ports: List[Int]):
    val strength: Int = ports.sum
  end Component

  def bridgeOfMaximumStrength(components: List[Component]): Int =
    def loop(components: List[Component], bridge: List[Component], nextPort: Int): Int =
      if !components.exists(_.ports.contains(nextPort)) then
        bridge.map(_.strength).sum
      else
        components.filter(_.ports.contains(nextPort)).map(c =>
          loop(
            components.filterNot(_.==(c)),
            bridge :+ c,
            c.ports.filterNot(_.==(nextPort)).headOption.getOrElse(nextPort)
          )
        ).max

    loop(components, List.empty[Component], 0)

  def longestBridgeOfMaximumStrength(components: List[Component]): Int =
    def loop(components: List[Component], bridge: List[Component], nextPort: Int): (Int, Int) =
      if !components.exists(_.ports.contains(nextPort)) then
        (bridge.length, bridge.map(_.strength).sum)
      else
        components.filter(_.ports.contains(nextPort)).map(c =>
          loop(
            components.filterNot(_.==(c)),
            bridge :+ c,
            c.ports.filterNot(_.==(nextPort)).headOption.getOrElse(nextPort)
          )
        ).maxBy(x => (x._1, x._2))

    loop(components, List.empty[Component], 0)._2

  def handleLines(lines: List[String]): List[Component] =
    lines.map:
      case s"$i/$j" => Component(List(i.toInt, j.toInt))

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day24
