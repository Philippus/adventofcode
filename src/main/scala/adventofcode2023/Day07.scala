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

    override def <(that: Card): Boolean = that match
      case `2` => false
      case `3` => this == `2`
      case `4` => this <= `3`
      case `5` => this <= `4`
      case `6` => this <= `5`
      case `7` => this <= `6`
      case `8` => this <= `7`
      case `9` => this <= `8`
      case `T` => this <= `9`
      case `J` => this <= `T`
      case `Q` => this <= `J`
      case `K` => this <= `Q`
      case `A` => this <= `K`

  object Card:
    def fromChar(c: Char): Card =
      c match
        case '2' => `2`
        case '3' => `3`
        case '4' => `4`
        case '5' => `5`
        case '6' => `6`
        case '7' => `7`
        case '8' => `8`
        case '9' => `9`
        case 'T' => `T`
        case 'J' => `J`
        case 'Q' => `Q`
        case 'K' => `K`
        case 'A' => `A`
  end Card

  class Hand(val cards: Seq[Card]) extends Ordered[Hand]:
    import HandType.*
    def compare(that: Hand): Int =
      if this == that then 0
      else if this < that then -1
      else 1

    override def <(that: Hand): Boolean =
      (determineType(this.cards.map(_.toString).mkString), determineType(that.cards.map(_.toString).mkString)) match
        case (a, b) if a == b  =>
          // compare high cards
          val zipped      = this.cards.zip(that.cards)
          val comparisons =
            for
              zip <- zipped
            yield zip._1.compare(zip._2)
          val comparison  = comparisons.find(!_.==(0)).get
          if comparison == -1 then true else false
        case (_, HighCard)     => false
        case (_, OnePair)      => determineType(this.cards.map(_.toString).mkString) == HighCard
        case (_, TwoPair)      => determineType(this.cards.map(_.toString).mkString) <= OnePair
        case (_, ThreeOfAKind) => determineType(this.cards.map(_.toString).mkString) <= TwoPair
        case (_, FullHouse)    => determineType(this.cards.map(_.toString).mkString) <= ThreeOfAKind
        case (_, FourOfAKind)  => determineType(this.cards.map(_.toString).mkString) <= FullHouse
        case (_, FiveOfAKind)  => determineType(this.cards.map(_.toString).mkString) <= FourOfAKind

    override def toString =
      cards.map(_.toString).mkString
  object Hand:
    def fromString(s: String): Hand =
      val seq = s.map(Card.fromChar)
      Hand(seq)

  end Hand

  enum JokerCard extends Ordered[JokerCard]:
    case `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `T`, `J`, `Q`, `K`, `A`

    import JokerCard._

    def compare(that: JokerCard): Int =
      if this == that then 0
      else if this < that then -1
      else 1

    override def <(that: JokerCard): Boolean = that match
      case `J` => false
      case `2` => this == `J`
      case `3` => this <= `2`
      case `4` => this <= `3`
      case `5` => this <= `4`
      case `6` => this <= `5`
      case `7` => this <= `6`
      case `8` => this <= `7`
      case `9` => this <= `8`
      case `T` => this <= `9`
      case `Q` => this <= `T`
      case `K` => this <= `Q`
      case `A` => this <= `K`

  object JokerCard:
    def fromChar(c: Char): JokerCard =
      c match
        case '2' => `2`
        case '3' => `3`
        case '4' => `4`
        case '5' => `5`
        case '6' => `6`
        case '7' => `7`
        case '8' => `8`
        case '9' => `9`
        case 'T' => `T`
        case 'J' => `J`
        case 'Q' => `Q`
        case 'K' => `K`
        case 'A' => `A`
  end JokerCard

  class JokerHand(val cards: Seq[JokerCard]) extends Ordered[JokerHand]:
    import HandType.*

    def compare(that: JokerHand): Int =
      if this == that then 0
      else if this < that then -1
      else 1

    override def <(that: JokerHand): Boolean =
      (
        determineJokerType(this.cards.map(_.toString).mkString),
        determineJokerType(that.cards.map(_.toString).mkString)
      ) match
        case (a, b) if a == b  =>
          // compare high cards
          val zipped      = this.cards.zip(that.cards)
          val comparisons =
            for
              zip <- zipped
            yield zip._1.compare(zip._2)
          val comparison  = comparisons.find(!_.==(0)).get
          if comparison == -1 then true else false
        case (_, HighCard)     => false
        case (a, OnePair)      => a == HighCard
        case (a, TwoPair)      => a <= OnePair
        case (a, ThreeOfAKind) => a <= TwoPair
        case (a, FullHouse)    => a <= ThreeOfAKind
        case (a, FourOfAKind)  => a <= FullHouse
        case (a, FiveOfAKind)  => a <= FourOfAKind

    override def toString =
      cards.map(_.toString).mkString

  object JokerHand:
    def fromString(s: String): JokerHand =
      val seq = s.map(JokerCard.fromChar)
      JokerHand(seq)

  end JokerHand
  //
  enum HandType extends Ordered[HandType]:
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard
    //
    def compare(that: HandType): Int =
      if this == that then 0
      else if this < that then -1
      else 1

    override def <(that: HandType): Boolean = that match
      case HighCard     => false
      case OnePair      => this == HighCard
      case TwoPair      => this <= OnePair
      case ThreeOfAKind => this <= TwoPair
      case FullHouse    => this <= ThreeOfAKind
      case FourOfAKind  => this <= FullHouse
      case FiveOfAKind  => this <= FourOfAKind
  end HandType

  val labels = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')

  def determineType(hand: String): HandType =
    val freq =
      for
        label <- labels
      yield hand.count(_.==(label))
    if freq.exists(_.==(5)) then
      HandType.FiveOfAKind
    else if freq.exists(_.==(4)) then
      HandType.FourOfAKind
    else if (freq.exists(_.==(3)) && freq.exists(_.==(2))) then
      HandType.FullHouse
    else if freq.exists(_.==(3)) then
      HandType.ThreeOfAKind
    else if freq.count(_.==(2)) == 2 then
      HandType.TwoPair
    else if freq.exists(_.==(2)) then
      HandType.OnePair
    else
      HandType.HighCard

  def determineJokerType(hand: String): HandType =
    val freq    =
      for
        label <- labels
        if label != 'J'
      yield (JokerCard.fromChar(label), hand.count(_.==(label)))
    val highest = freq.maxBy(f => (f._2, f._1))._1
    determineType(hand.replace("J", highest.toString))

  def readLine(line: String): (Hand, Int) =
    line match
      case s"$h $b" => (Hand.fromString(h), b.toInt)

  def readJokerLine(line: String): (JokerHand, Int) =
    line match
      case s"$h $b" => (JokerHand.fromString(h), b.toInt)

  def readDocument =
    Using.resource(Source.fromResource("2023/day07input.txt")): source =>
      val handsAndBids = source.getLines.toSeq.map(readLine)
      val withIndex    = handsAndBids.sortBy(_._1).zipWithIndex
      withIndex.map(e => e._1._2 * (e._2 + 1)).sum

  def readDocumentPartTwo =
    Using.resource(Source.fromResource("2023/day07input.txt")): source =>
      val handsAndBids = source.getLines.toSeq.map(readJokerLine)
      val withIndex    = handsAndBids.sortBy(_._1).zipWithIndex
      withIndex.map(e => e._1._2 * (e._2 + 1)).sum
end Day07
