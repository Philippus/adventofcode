package adventofcode2023.day4

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day4:
  def calculateScratchCardScore(line: String): Int =
    val (cardNumber, winningNumbers, cardNumbers) = line match
      case s"Card $c: $w | $n" => (c.trim, w.trim.split("\\s+"), n.trim.split("\\s+"))
    val myWinningNumbers                          = cardNumbers.intersect(winningNumbers)

    if myWinningNumbers.isEmpty
    then 0
    else scala.math.pow(2, myWinningNumbers.length - 1).toInt

  def determineCopies(line: String): Seq[Int] =
    val (cardNumber, winningNumbers, cardNumbers) = line match
      case s"Card $c: $w | $n" => (c.trim, w.trim.split("\\s+"), n.trim.split("\\s+"))
    val myWinningNumbers                          = cardNumbers.intersect(winningNumbers)

    Range(cardNumber.toInt + 1, cardNumber.toInt + 1 + myWinningNumbers.length)

  def readInputDocument: Int =
    Using.resource(Source.fromResource("day4input.txt")):
      _.getLines().map(line => calculateScratchCardScore(line)).sum

  def calculateNumberOfCardsForInputDocument: Int =
    @tailrec
    def calculateNumberOfCardsForDocumentHelper(input: Seq[Int], lines: Seq[String], acc: Int): Int =
      if input.isEmpty then acc
      else
        val copies = determineCopies(lines.apply(input.head - 1))
        calculateNumberOfCardsForDocumentHelper(input.tail ++ copies, lines, acc + 1)

    Using.resource(Source.fromResource("day4input.txt")): source =>
      val lines = source.getLines.toSeq
      calculateNumberOfCardsForDocumentHelper(Range.inclusive(1, lines.length), lines, 0)
end Day4
