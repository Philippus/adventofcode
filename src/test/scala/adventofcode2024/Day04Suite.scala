package adventofcode2024

import scala.io.Source
import scala.util.{Try, Using}

import adventofcode2024.Day04.{importLines, *}
import munit.FunSuite

class Day04Suite extends FunSuite:
  private val tinySampleGrid = Array(
    "..X...".toCharArray,
    ".SAMX.".toCharArray,
    ".A..A.".toCharArray,
    "XMAS.S".toCharArray,
    ".X....".toCharArray
  )

  private val sampleGrid = Array(
    "MMMSXXMASM".toCharArray,
    "MSAMXMSMSA".toCharArray,
    "AMXSXMAAMM".toCharArray,
    "MSAMASMSMX".toCharArray,
    "XMASAMXAMM".toCharArray,
    "XXAMMXXAMA".toCharArray,
    "SMSMSASXSS".toCharArray,
    "SAXAMASAAA".toCharArray,
    "MAMMMXMMMM".toCharArray,
    "MXMXAXMASX".toCharArray
  )

  test("counts xmas-es for the tiny sample"):
    assertEquals(countXmas(tinySampleGrid), 4)

  test("counts xmas-es for the sample input"):
    assertEquals(countXmas(sampleGrid), 18)

  test("counts xmas-es for the input"):
    importLines()
    assertEquals(countXmas(grid), 2547)

  private val tinyMasGrid = Array(
    "M.S".toCharArray,
    ".A.".toCharArray,
    "M.S".toCharArray
  )

  test("counts mas-es for the tiny sample"):
    assertEquals(countMas(tinyMasGrid), 1)

  test("counts mas-es for the sample input"):
    assertEquals(countMas(sampleGrid), 9)

  test("counts mas-es for the input"):
    importLines()
    assertEquals(countMas(grid), 1939)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day04Suite
