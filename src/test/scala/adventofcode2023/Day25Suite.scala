package adventofcode2023

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("should work the example"):
    def importLines(): Seq[String] =
      Using.resource(Source.fromResource("2023/day25sampleinput.txt")):
        _.getLines().toSeq

    val connections = importLines().flatMap(handleLine)
    val components  = getComponents(connections.toSet)

    assertEquals(findMultipleOfGroupSizes(components, connections, 5), 54)

  test("should work the input file"):
    val connections = importLines().flatMap(handleLine)
    val components  = getComponents(connections.toSet)

    assertEquals(findMultipleOfGroupSizes(components, connections, 3), 554064)
end Day25Suite
