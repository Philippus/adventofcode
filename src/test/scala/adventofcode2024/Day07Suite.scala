package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("determines total calibration result for the sample input"):
    val lines = importSampleLines()
    assertEquals(calibrationResult(lines), 3749L)

  test("determines total calibration result for the input"):
    val lines = importLines()
    assertEquals(calibrationResult(lines), 1985268524462L)

  test("determines total calibration result with concatenation for the sample input"):
    val lines = importSampleLines()
    assertEquals(calibrationResult(lines, withConcat = true), 11387L)

  test("determines total calibration result with concatenation for the input"):
    val lines = importLines()
    assertEquals(calibrationResult(lines, withConcat = true), 150077710195188L)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day07Suite
