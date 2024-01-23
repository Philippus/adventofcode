package adventofcode2020

import adventofcode2020.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("counts the number of answered questions"):
    val answers = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
    assertEquals(countAnswers(answers), 11)

  test("counts the number of answered questions in input"):
    val answers = readInputFile()
    assertEquals(countAnswers(answers), 6335)

  test("counts the number of questions everyone answered"):
    val answers = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
    assertEquals(countQuestionsEveryoneAnswered(answers), 6)

  test("counts the number of questions everyone answered in input"):
    val answers = readInputFile()
    assertEquals(countQuestionsEveryoneAnswered(answers), 3392)
end Day06Suite
