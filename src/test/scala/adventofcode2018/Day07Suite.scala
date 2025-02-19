package adventofcode2018

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

  test("completes steps with 1 workers for the sample"):
    assertEquals(followInstructionsWithMultipleWorkers(readSteps(importSampleLines()), 1, 0)._1, "CABDFE")

  test("completes steps with 1 workers for the input"):
    assertEquals(followInstructionsWithMultipleWorkers(readSteps(importLines()), 1, 0)._1, "HEGMPOAWBFCDITVXYZRKUQNSLJ")

  test("completes steps with 2 workers for the sample"):
    assertEquals(followInstructionsWithMultipleWorkers(readSteps(importSampleLines()), 2, 0)._2, 15)

  test("completes steps with 5 workers for the input"):
    assertEquals(followInstructionsWithMultipleWorkers(readSteps(importLines()), 5, 60)._2, 1226)

  def importSampleLines(): Seq[String] =
    Using.resource(Source.fromResource("2018/day07sampleinput.txt")):
      _.getLines().toSeq
end Day07Suite
