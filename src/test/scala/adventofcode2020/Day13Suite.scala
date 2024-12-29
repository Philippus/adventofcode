package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("determines earliest bus for the sample"):
    val (timestamp, buses) = importSampleLines()
    assertEquals(determineEarliestBus(timestamp, buses), 295L)

  test("determines earliest bus for the input"):
    val (timestamp, buses) = importLines()
    assertEquals(determineEarliestBus(timestamp, buses), 203L)

  def importSampleLines(): (Long, Seq[Int]) =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      val list = source.getLines().toList
      (list.head.toLong, list.last.split(',').filterNot(_.==("x")).map(_.toInt))
end Day13Suite
