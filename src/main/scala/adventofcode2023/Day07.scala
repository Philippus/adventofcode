package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day07.HandType
import adventofcode2023.Day07.HandType.{FiveOfAKind, FourOfAKind}

object Day07:
  enum Card extends Ordered[Card]:
    case `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `T`, `J`, `Q`, `K`, `A`

    import Card._
    def compare(that: Card): Int =
      if this == that then 0
      else if this < that then -1
      else 1

    override def <(that: Card): Boolean =
      Card.values.indexOf(this) < Card.values.indexOf(that)

  object Card:
    private val chars      = Seq('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
    private val jokerOrder = Seq(`J`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `T`, `Q`, `K`, `A`)

    def fromChar(c: Char): Card =
      chars.zip(Card.values).collectFirst { case v if v._1 == c => v._2 }.get

    def jokerCompare(c1: Card, c2: Card): Int =
      jokerOrder.indexOf(c1) - jokerOrder.indexOf(c2)
  end Card

  class Hand(val cards: Seq[Card], jokerRule: Boolean = false) extends Ordered[Hand]:
    import HandType.*
    def compare(that: Hand): Int =
      if this == that then 0
      else if this < that then -1
      else 1

    override def <(that: Hand): Boolean =
      val (thisHandType, thatHandType) =
        if (jokerRule)
          (determineJokerType(this), determineJokerType(that))
        else
          (determineType(this), determineType(that))
      (thisHandType, thatHandType) match
        case (a, b) if a == b =>
          if (jokerRule) then
            val zipped      = this.cards.zip(that.cards)
            val comparisons =
              for
                zip <- zipped
              yield Card.jokerCompare(zip._1, zip._2)
            comparisons.find(!_.==(0)).exists(_.<(0))
          else
            val zipped      = this.cards.zip(that.cards)
            val comparisons =
              for
                zip <- zipped
              yield zip._1.compare(zip._2)
            comparisons.find(!_.==(0)).exists(_.<(0))
        case (a, b)           => a < b

    override def toString: String =
      cards.map(_.toString).mkString

  object Hand:
    def fromString(s: String, jokerRule: Boolean = false): Hand =
      val seq = s.map(Card.fromChar)
      if jokerRule then
        Hand(seq, true)
      else
        Hand(seq)
  end Hand

  enum HandType extends Ordered[HandType]:
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

    def compare(that: HandType): Int =
      if this == that then 0
      else if this < that then -1
      else 1

    override def <(that: HandType): Boolean = HandType.values.indexOf(this) < HandType.values.indexOf(that)
  end HandType

  def determineType(hand: Hand): HandType =
    hand.cards.groupBy(identity).view.mapValues(_.length).values.toSeq.sorted.reverse match
      case 5 :: _      => HandType.FiveOfAKind
      case 4 :: _      => HandType.FourOfAKind
      case 3 :: 2 :: _ => HandType.FullHouse
      case 3 :: _      => HandType.ThreeOfAKind
      case 2 :: 2 :: _ => HandType.TwoPair
      case 2 :: _      => HandType.OnePair
      case _           => HandType.HighCard

  def determineJokerType(hand: Hand): HandType =
    if hand.cards.count(_.==(Card.`J`)) == 5 then
      HandType.FiveOfAKind
    else
      val cardsWithoutJoker = hand.cards.filterNot(_.==(Card.`J`))
      val cardToReplace = cardsWithoutJoker.groupBy(identity).view.mapValues(_.length).maxBy(_._2)._1
      determineType(Hand(cardsWithoutJoker ++ Seq.fill(5 - cardsWithoutJoker.length)(cardToReplace)))

  def readLine(line: String, jokerRule: Boolean): (Hand, Int) =
    line match
      case s"$h $b" => (Hand.fromString(h, jokerRule), b.toInt)

  def readDocument =
    Using.resource(Source.fromResource("2023/day07input.txt")): source =>
      val handsAndBids = source.getLines.toSeq.map(l => readLine(l, false))
      val withIndex    = handsAndBids.sortBy(_._1).zipWithIndex
      withIndex.map(e => e._1._2 * (e._2 + 1)).sum

  def readDocumentPartTwo =
    Using.resource(Source.fromResource("2023/day07input.txt")): source =>
      val handsAndBids = source.getLines.toSeq.map(l => readLine(l, true))
      val withIndex    = handsAndBids.sortBy(_._1).zipWithIndex
      withIndex.map(e => e._1._2 * (e._2 + 1)).sum
end Day07
