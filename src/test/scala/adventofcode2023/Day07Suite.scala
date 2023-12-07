package adventofcode2023

import scala.collection.immutable.Seq

import adventofcode2023.Day07.*
import adventofcode2023.Day07.HandType.*

import munit.FunSuite

class Day07Suite extends FunSuite:
  test("determines hand type"):
    assertEquals(determineType(Hand.fromString("AAAAA")), FiveOfAKind)
    assertEquals(determineType(Hand.fromString("AA8AA")), FourOfAKind)
    assertEquals(determineType(Hand.fromString("23332")), FullHouse)
    assertEquals(determineType(Hand.fromString("TTT98")), ThreeOfAKind)
    assertEquals(determineType(Hand.fromString("23432")), TwoPair)
    assertEquals(determineType(Hand.fromString("A23A4")), OnePair)
    assertEquals(determineType(Hand.fromString("23456")), HighCard)

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
        Hand.fromString("32T3K", true),
        Hand.fromString("T55J5", true),
        Hand.fromString("KK677", true),
        Hand.fromString("KTJJT", true),
        Hand.fromString("QQQJA", true)
      ).sorted.map(_.toString),
      List("32T3K", "KK677", "T55J5", "QQQJA", "KTJJT")
    )

  test("determines hand type part two"):
    assertEquals(determineJokerType(Hand.fromString("JJJJJ")), FiveOfAKind)
    assertEquals(determineJokerType(Hand.fromString("JKKK2")), FourOfAKind)
    assertEquals(determineJokerType(Hand.fromString("QQQQ2")), FourOfAKind)

  test("can compare hands part two"):
    assert(Hand.fromString("JKKK2", true) < Hand.fromString("QQQQ2", true))

  test("gets expected winnings in part two"):
    val handsAndBids = Seq(
      (Hand.fromString("32T3K", true), 765),
      (Hand.fromString("T55J5", true), 684),
      (Hand.fromString("KK677", true), 28),
      (Hand.fromString("KTJJT", true), 220),
      (Hand.fromString("QQQJA", true), 483)
    )
    val withIndex    = handsAndBids.sortBy(_._1).zipWithIndex
    val winnings     = withIndex.map(e => e._1._2 * (e._2 + 1)).sum
    assertEquals(winnings, 5905)

  test("gets expected results for the input part two"):
    assertEquals(readDocumentPartTwo, 248909434)

end Day07Suite
