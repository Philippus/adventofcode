package adventofcode2018

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day08:
  case class Node(nodes: List[Node], metadataEntries: List[Int])

  private def buildNodeTree(input: Vector[Int]) =
    def loop(cursorStart: Int): (Node, Int) =
      var cursor           = cursorStart
      var nChildNodes      = input(cursor)
      var nMetadataEntries = input(cursor + 1)
      cursor += 2
      val childNodes       = mutable.ListBuffer.empty[Node]
      while nChildNodes > 0 do
        val (childNode, newCursor) = loop(cursor)
        childNodes.addOne(childNode)
        nChildNodes -= 1
        cursor = newCursor
      val metadataEntries  = mutable.ListBuffer.empty[Int]
      while nMetadataEntries > 0 do
        metadataEntries.addOne(input(cursor))
        nMetadataEntries -= 1
        cursor += 1
      (Node(childNodes.toList, metadataEntries.toList), cursor)
    loop(0)

  def sumOfMetadataEntries(input: Vector[Int]): Long =
    def walkTreeForSum(node: Node): Long =
      node.nodes.map(node => walkTreeForSum(node)).sum + node.metadataEntries.sum

    val nodeTree = buildNodeTree(input)._1
    walkTreeForSum(nodeTree)

  def rootNodeValue(input: Vector[Int]): Long =
    def nodeValue(node: Node): Long =
      if node.nodes.isEmpty then
        node.metadataEntries.sum
      else
        val metaDataEntries = node.metadataEntries
        metaDataEntries.map:
          case i if i == 0                => 0L
          case i if i > node.nodes.length => 0L
          case i                          => nodeValue(node.nodes(i - 1))
        .sum

    val nodeTree = buildNodeTree(input)._1
    nodeValue(nodeTree)

  def importLines(): Vector[Int] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next().split(' ').map(_.toInt).toVector
end Day08
