package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day08:
  case class Choice(source: String, left: String, right: String)

  def calculateSteps(instructions: String, startNodes: Seq[String], nodes: Set[Choice], endCondition: String): BigInt =
    @tailrec
    def helper(instructionsLeft: String, currentNode: String, acc: Int): BigInt =
      if currentNode.endsWith(endCondition) then
        acc
      else
        val nextNode = instructionsLeft.head match
          case 'L' => nodes.find(_.source == currentNode).map(_.left).get
          case 'R' => nodes.find(_.source == currentNode).map(_.right).get
        helper(if instructionsLeft.tail.nonEmpty then instructionsLeft.tail else instructions, nextNode, acc + 1)

    def lcm(a: BigInt, b: BigInt): BigInt =
      @tailrec
      def gdc(a: BigInt, b: BigInt): BigInt =
        if b == 0 then a else gdc(b, a % b)
      a * (b / gdc(a, b))

    startNodes.map(helper(instructions, _, 0))
      .foldLeft(BigInt(1))((acc, x) => lcm(acc, x))

  def readLine(line: String): Choice =
    line match
      case s"$source = ($left, $right)" => Choice(source, left, right)

  def readDocument: BigInt =
    Using.resource(Source.fromResource("2023/day08input.txt")): source =>
      val lines        = source.getLines.toSeq
      val instructions = lines.head
      val nodes        = lines.drop(2).map(readLine).toSet
      calculateSteps(instructions, Seq("AAA"), nodes, "ZZZ")

  def readDocumentForGhosts: BigInt =
    Using.resource(Source.fromResource("2023/day08input.txt")): source =>
      val lines        = source.getLines.toSeq
      val instructions = lines.head
      val nodes        = lines.drop(2).map(readLine).toSet
      calculateSteps(instructions, nodes.map(_.source).filter(_.endsWith("A")).toSeq, nodes, "Z")
end Day08
