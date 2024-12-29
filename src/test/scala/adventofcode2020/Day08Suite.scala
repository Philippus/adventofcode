package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("runs boot code for the sample"):
    val bootCode = importSampleLines()
    assertEquals(runBootCode(bootCode), 5)

  test("runs boot code for the input"):
    val bootCode = importLines()
    assertEquals(runBootCode(bootCode), 2058)

  test("fixes the boot code for the sample"):
    val bootCode = importSampleLines()
    assertEquals(fixBootCode(bootCode), 8)

  test("fixes the boot code for the input"):
    val bootCode = importLines()
    assertEquals(fixBootCode(bootCode), 1000)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day08Suite
