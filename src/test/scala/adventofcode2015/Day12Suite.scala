package adventofcode2015

import adventofcode2015.Day12.*
import munit.FunSuite
import spray.json._

class Day12Suite extends FunSuite:
  val json: JsValue = readInputFile().parseJson
  test("works for the input file"):
    assertEquals(sumNumbers(json), 156366)

  test("works for the input file part two"):
    assertEquals(sumNumbers(json, true), 96852)
end Day12Suite
