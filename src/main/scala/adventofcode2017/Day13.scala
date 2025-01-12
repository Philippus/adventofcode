package adventofcode2017

import scala.io.Source
import scala.util.Using

object Day13:
  def severity(firewallLayers: List[(Int, Int)], delay: Int = 0): (Boolean, Int) =
    val severityValues = firewallLayers.collect:
      case (layer, depth) if (layer + delay) % ((depth - 1) * 2) == 0 =>
        layer * depth

    if severityValues.nonEmpty then
      (true, severityValues.sum)
    else
      (false, 0)

  def calculateDelayToNotGetCaught(firewallLayers: List[(Int, Int)]): Int =
    var j      = 0
    var caught = true
    while caught do
      j += 1
      caught = severity(firewallLayers, delay = j)._1
    j

  def handleLines(lines: List[String]): List[(Int, Int)] =
    lines.map:
      case s"$i: $v" => i.toInt -> v.toInt

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day13
