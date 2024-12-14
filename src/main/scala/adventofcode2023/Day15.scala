package adventofcode2023

import scala.io.Source
import scala.util.Using

object Day15:
  def calculateHash(hash: String): Long =
    var currentValue = 0L
    hash.foreach: char =>
      currentValue += char.toInt.toLong
      currentValue = currentValue * 17L
      currentValue = currentValue % 256L
    currentValue

  def sumOfHashes(steps: String): Long =
    steps.split(',').toSeq.map(calculateHash).sum

  def importLines(): String =
    Using.resource(Source.fromResource(s"2023/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.head
end Day15
