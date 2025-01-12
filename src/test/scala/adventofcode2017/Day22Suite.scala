package adventofcode2017

import scala.io.Source
import scala.util.Using

import adventofcode2017.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("infects nodes with the virus for the sample"):
    val initialNodes = handleLines(importSampleLines())
    assertEquals(infectNodes(initialNodes, 70), 41)
    assertEquals(infectNodes(initialNodes, 10000), 5587)

  test("infects nodes with the virus for the input"):
    val initialNodes = handleLines(importLines())
    assertEquals(infectNodes(initialNodes, 10000), 5399)

  test("infects nodes with the evolved virus for the sample"):
    val initialNodes = handleLines(importSampleLines())
    assertEquals(infectNodes(initialNodes, 100, true), 26)
    assertEquals(infectNodes(initialNodes, 10000000, true), 2511944)

  test("infects nodes with the evolved virus for the input"):
    val initialNodes = handleLines(importLines())
    assertEquals(infectNodes(initialNodes, 10000000, true), 2511776)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day22Suite
