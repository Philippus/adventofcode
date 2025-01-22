package adventofcode2017

import scala.io.Source
import scala.util.Using

import adventofcode2017.Day21.*
import munit.FunSuite

class Day21Suite extends FunSuite:
  test("counts pixels that stay on after iterations"):
    val ruleBook = handleLines(importLines())
    assertEquals(pixelsOnAfterIterations(ruleBook, 5, ".#.\n..#\n###"), 162)
    assertEquals(pixelsOnAfterIterations(ruleBook, 18, ".#.\n..#\n###"), 2264586) // takes a loooong time

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day21Suite
