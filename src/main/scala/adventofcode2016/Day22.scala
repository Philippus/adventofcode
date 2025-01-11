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

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(nodes: Seq[Node]): String =
    createGrid(nodes.maxBy(_.x).x + 1, nodes.maxBy(_.y).y + 1).map:
      case (x, y) => (if x == 0 then "\n" else "") + {
        val usePerc = nodes.find(node => node.x == x && node.y == y).get.usePerc
        if (x, y) == (nodes.maxBy(_.x).x, 0) then 'G' else if usePerc == 0 then '_' else if usePerc > 90 then '#' else '.'
      }
    .mkString

  def parseLine(line: String): Node =
    line match
      case s"/dev/grid/node-x$x-y$y ${size}T ${used}T ${avail}T ${usePerc}%" =>
        Node(x.toInt, y.toInt, size.strip.toInt, used.strip.toInt, avail.strip.toInt, usePerc.strip.toInt)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.drop(2)
end Day22
