package adventofcode2016

import scala.io.Source
import scala.util.Using

object Day22:
  case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int, usePerc: Int)

  def viablePairs(nodes: Seq[Node]): Int =
    (for
      node <- nodes
      if node.used != 0
      if nodes.filterNot(_.==(node)).exists(otherNode => node.used <= otherNode.avail)
    yield node).length

  def parseLine(line: String): Node =
    line match
      case s"/dev/grid/node-x$x-y$y ${size}T ${used}T ${avail}T ${usePerc}%" =>
        Node(x.toInt, y.toInt, size.strip.toInt, used.strip.toInt, avail.strip.toInt, usePerc.strip.toInt)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.drop(2)
end Day22
