package adventofcode2023

import scala.collection.immutable.Seq

import adventofcode2023.Day07.*
import adventofcode2023.Day07.HandType._
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("determines hand type"):
    assertEquals(determineType("AAAAA"), FiveOfAKind)
    assertEquals(determineType("AA8AA"), FourOfAKind)
    assertEquals(determineType("23332"), FullHouse)
    assertEquals(determineType("TTT98"), ThreeOfAKind)
    assertEquals(determineType("23432"), TwoPair)
    assertEquals(determineType("A23A4"), OnePair)
    assertEquals(determineType("23456"), HighCard)

  test("can compare hands"):
    assert(Hand.fromString("AAAAA") > Hand.fromString("AA8AA"))
    assert(Hand.fromString("23332") > Hand.fromString("A23A4"))
    assert(Hand.fromString("23456") < Hand.fromString("TTT98"))
    assert(Hand.fromString("23456") < Hand.fromString("34567"))
    assert(Hand.fromString("34567") > Hand.fromString("23456"))

  test("can sort hands"):
    assertEquals(
      Seq(
        Hand.fromString("32T3K"),
        Hand.fromString("T55J5"),
        Hand.fromString("KK677"),
        Hand.fromString("KTJJT"),
        Hand.fromString("QQQJA")
      ).sorted.map(_.toString),
      List("32T3K", "KTJJT", "KK677", "T55J5", "QQQJA")
    )

  test("gets expected results for the input"):
    assertEquals(readDocument, 250474325)

  test("can sort hands with joker rule"):
    assertEquals(
      Seq(
        JokerHand.fromString("32T3K"),
        JokerHand.fromString("T55J5"),
        JokerHand.fromString("KK677"),
        JokerHand.fromString("KTJJT"),
        JokerHand.fromString("QQQJA")
      ).sorted.map(_.toString),
      List("32T3K", "KK677", "T55J5", "QQQJA", "KTJJT")
    )

  test("determines hand type part two"):
    assertEquals(determineJokerType("JJJJJ"), FiveOfAKind)
    assertEquals(determineJokerType("JKKK2"), FourOfAKind)
    assertEquals(determineJokerType("QQQQ2"), FourOfAKind)

  test("can compare hands part two"):
    assert(JokerHand.fromString("JKKK2") < JokerHand.fromString("QQQQ2"))

  test("gets expected winnings in part two"):
    val handsAndBids = Seq(
      (JokerHand.fromString("32T3K"), 765),
      (JokerHand.fromString("T55J5"), 684),
      (JokerHand.fromString("KK677"), 28),
      (JokerHand.fromString("KTJJT"), 220),
      (JokerHand.fromString("QQQJA"), 483)
    )
    val withIndex    = handsAndBids.sortBy(_._1).zipWithIndex
    val winnings     = withIndex.map(e => e._1._2 * (e._2 + 1)).sum
    assertEquals(winnings, 5905)

  test("gets expected results for the input part two"):
    assertEquals(readDocumentPartTwo, 248909434)

end Day07Suite
