package adventofcode2023

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.day4.Day4.*
import munit.FunSuite

class Day4Suite extends FunSuite:
  test("calculate score for a card"):
    assertEquals(calculateScratchCardScore("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"), 8)
    assertEquals(calculateScratchCardScore("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"), 2)
    assertEquals(calculateScratchCardScore("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"), 2)
    assertEquals(calculateScratchCardScore("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"), 1)
    assertEquals(calculateScratchCardScore("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"), 0)
    assertEquals(calculateScratchCardScore("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"), 0)

  test("calculates core for a document"):
    assertEquals(readInputDocument, 19855)

  test("determines copies for a card"):
    assertEquals(determineCopies("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"), Seq(2, 3, 4, 5))

  test("calculates number of cards for a document"):
    assertEquals(calculateNumberOfCardsForInputDocument, 10378710)
end Day4Suite
