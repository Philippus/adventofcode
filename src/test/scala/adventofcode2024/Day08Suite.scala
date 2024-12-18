package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("draws the map for the sample"):
    val antennas = handleLines(importSampleLines())
    val str      = drawGrid(antennas, Seq.empty, 12, 12)
    println(s"$str")

  test("creates antinodes for a pair"):
    val antennas = Seq(Antenna(4, 3, 'a'), Antenna(5, 5, 'a'))
    assertEquals(createAntinodesForPair(antennas.head, antennas.last, 12, 12), Set(Antinode(3, 1), Antinode(6, 7)))

  test("count distinct antinodes on map for the sample"):
    val antennas = handleLines(importSampleLines())
    assertEquals(countDistinctAntinodesOnMap(antennas, 12, 12), 14)

  test("count distinct antinodes on map for the input"):
    val antennas = handleLines(importLines())
    assertEquals(countDistinctAntinodesOnMap(antennas, 50, 50), 381)

  test("count distinct antinodes with resonant harmonics on map for the sample"):
    val antennas = handleLines(importSampleLines())
    assertEquals(countDistinctAntinodesWithResonantHarmonicsOnMap(antennas, 12, 12), 34)

  test("count distinct antinodes with resonant harmonics on map for the input"):
    val antennas = handleLines(importLines())
    assertEquals(countDistinctAntinodesWithResonantHarmonicsOnMap(antennas, 50, 50), 1184)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day08Suite
