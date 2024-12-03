package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("adds up multiplications for the sample input"):
    assertEquals(addUpMultiplications(importSampleLines().mkString("a"), 0L), 161L)

  test("adds up multiplications for the input"):
    assertEquals(addUpMultiplications(importLines().mkString("a"), 0L), 180233229L)

  test("adds up enabled multiplications for the sample input"):
    assertEquals(addUpEnabledMultiplications(importSampleLines().mkString("a"), 0L, true), 48L)

  test("adds up enabled multiplications for the input"):
    assertEquals(addUpEnabledMultiplications(importLines().mkString("a"), 0L, true), 95411583L)

  def importSampleLines(): Seq[String] =
    Using.resource(Source.fromResource("2024/day03sampleinput.txt")): source =>
      source.getLines().toSeq
end Day03Suite
