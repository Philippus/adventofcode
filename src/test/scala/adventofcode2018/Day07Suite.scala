package adventofcode2018

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2018.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("reacts polymer for the sample"):
    val result = bootstrap(readSteps(importSampleLines()))
    assertEquals(result, "CABDFE")

  test("reacts polymer for the input"):
    val result = bootstrap(readSteps(importLines()))
    assertEquals(result, "HEGMPOAWBFCDITVXYZRKUQNSLJ")

  def importSampleLines(): Seq[String] =
    Using.resource(Source.fromResource("2018/day07sampleinput.txt")):
      _.getLines().toSeq
end Day07Suite
