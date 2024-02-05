package adventofcode2021

import adventofcode2021.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("calculates syntax error"):
    assertEquals(syntaxErrorScore("{([(<{}[<>[]}>{[]{[(<()>"), 1197)

  test("calculates syntax error for the example"):
    val input =
      "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]".split(
        '\n'
      ).toSeq
    assertEquals(syntaxErrorScoreForSeq(input), 26397)

  test("calculates syntax error for the input"):
    val input = importLines()
    assertEquals(syntaxErrorScoreForSeq(input), 216297)

  test("scores incomplete lines"):
    assertEquals(scoreIncompleteLine("[({(<(())[]>[[{[]{<()<>>"), 288957L)

  test("calculates middle score for the example"):
    val input =
      "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]".split(
        '\n'
      ).toSeq
    assertEquals(middleScoreOfIncompleteLines(input), 288957L)

  test("calculates middle score for the input"):
    val input = importLines()
    assertEquals(middleScoreOfIncompleteLines(input), 2165057169L)
end Day10Suite
