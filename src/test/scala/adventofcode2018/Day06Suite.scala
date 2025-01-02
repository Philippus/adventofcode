package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("determines size of area that isn't infinite for the sample"):
    val lines = importSampleLines()
    assertEquals(sizeOfAreaThatIsntInfinite(handleLines(lines)), 17)

  test("determines size of area that isn't infinite for the input"):
    val lines = importLines()
    assertEquals(sizeOfAreaThatIsntInfinite(handleLines(lines)), 4290)

  test("finds a safe region for the sample"):
    val lines = importSampleLines()
    assertEquals(sizeOfRegion(handleLines(lines), 32), 16)

  test("finds a safe region for the input"):
    val lines = importLines()
    assertEquals(sizeOfRegion(handleLines(lines), 10000), 37318)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day06Suite
