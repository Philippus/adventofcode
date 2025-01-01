package adventofcode2022

import scala.io.Source
import scala.util.Using

import adventofcode2022.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("converts snafu to decimals"):
    assertEquals(snafuToDec("1=-0-2"), 1747L)

  test("converts decimals to snafu"):
    assertEquals(decToSnafu(8L), "2=")
    assertEquals(decToSnafu(2022L), "1=11-2")
    assertEquals(decToSnafu(12345L), "1-0---0")
    assertEquals(decToSnafu(314159265L), "1121-1110-1=0")

  test("sums snafus for the sample"):
    assertEquals(sumSnafus(importSampleLines()), "2=-1=0")

  test("sums snafus for the input"):
    assertEquals(sumSnafus(importLines()), "2-=2-0=-0-=0200=--21")

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day25Suite
