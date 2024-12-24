package adventofcode2024

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using

import adventofcode2024.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  override val munitTimeout = Duration(2, "d")
  test("prints blocks"):
    val diskmap = importSampleLines()
    val blocks  = createBlocks(diskmap)
    println(blocks.map(i => if i == -1 then "." else i.toString).mkString)

  test("moves blocks for the sample"):
    val diskmap     = importSampleLines()
    val blocks      = createBlocks(diskmap)
    val movedBlocks = moveBlocks(blocks)
    val checksum    = fileChecksum(movedBlocks)
    assertEquals(checksum, 1928L)

  test("moves blocks for the input"):
    val diskmap     = importLines()
    val blocks      = createBlocks(diskmap)
    val movedBlocks = moveBlocks(blocks)
    val checksum    = fileChecksum(movedBlocks)
    assertEquals(checksum, 6384282079460L)

  test("moves whole files for the sample"):
    val diskmap     = importSampleLines()
    val blocks      = createBlocks(diskmap)
    val movedBlocks = moveBlocksPartTwo(blocks)
    val checksum    = fileChecksum(movedBlocks)
    assertEquals(checksum, 2858L)

  test("moves files for the input"):
    val diskmap     = importLines()
    val blocks      = createBlocks(diskmap)
    val movedBlocks = moveBlocksPartTwo(blocks)
    val checksum    = fileChecksum(movedBlocks)
    assertEquals(checksum, 6408966547049L)

  def importSampleLines(): String =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().next()
end Day09Suite
