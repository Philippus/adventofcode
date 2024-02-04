package adventofcode2021

import adventofcode2021.Day04.*
import munit.FunSuite

class Day04Suite extends FunSuite:
  test("determines if a board is a winner"):
    val lines        =
      "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7".split(
        '\n'
      ).toSeq.filter(_.nonEmpty)
    val drawnNumbers = lines.head.split(',').map(_.toInt).toSeq
    val boards       = lines.tail.grouped(5).map(s =>
      Vector(s.map(s => s.trim.split("\\s+").toVector.map(_.toInt))*)
    ).toSeq.map(Board.apply)
    assertEquals(isWinner(boards.last, drawnNumbers.take(12)), true)

  test("determines score of a board"):
    val lines        =
      "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7".split(
        '\n'
      ).toSeq.filter(_.nonEmpty)
    val drawnNumbers = lines.head.split(',').map(_.toInt).toSeq
    val boards       = lines.tail.grouped(5).map(s =>
      Vector(s.map(s => s.trim.split("\\s+").toVector.map(_.toInt))*)
    ).toSeq.map(Board.apply)
    assertEquals(score(boards.last, drawnNumbers.take(12)), 4512)

  test("determines winning score for the example"):
    val lines        =
      "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7".split(
        '\n'
      ).toSeq.filter(_.nonEmpty)
    val drawnNumbers = lines.head.split(',').map(_.toInt).toSeq
    val boards       = lines.tail.grouped(5).map(s =>
      Vector(s.map(s => s.trim.split("\\s+").toVector.map(_.toInt))*)
    ).toSeq.map(Board.apply)
    assertEquals(determineWinningScore(boards, drawnNumbers), 4512)

  test("determines winning score for the input"):
    val (drawnNumbers, boards) = importLines()
    assertEquals(determineWinningScore(boards, drawnNumbers), 64084)

  test("determines score for last winning board for the example"):
    val lines        =
      "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7".split(
        '\n'
      ).toSeq.filter(_.nonEmpty)
    val drawnNumbers = lines.head.split(',').map(_.toInt).toSeq
    val boards       = lines.tail.grouped(5).map(s =>
      Vector(s.map(s => s.trim.split("\\s+").toVector.map(_.toInt))*)
    ).toSeq.map(Board.apply)
    assertEquals(determineLastToWin(boards, drawnNumbers), 1924)

  test("determines score for last winning board for the input"):
    val (drawnNumbers, boards) = importLines()
    assertEquals(determineLastToWin(boards, drawnNumbers), 12833)
end Day04Suite
