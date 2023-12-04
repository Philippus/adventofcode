package adventofcode2023.day4

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day4 {
  def calculateScratchCardScore(line: String): Int =
    @tailrec
    def calculateScore(numbers: Array[String], acc: Int): Int =
      if numbers.isEmpty then acc
      else if acc == 0 then calculateScore(numbers.tail, 1)
      else calculateScore(numbers.tail, acc * 2)
    val Array(card, numbers)                                  = line.split(": ")
    val Array(winningNumbersAsString, cardNumbersAsString)    = numbers.trim.split('|')
    val winningNumbers                                        = winningNumbersAsString.trim.split("\\s+")
    val cardNumbers                                           = cardNumbersAsString.trim.split("\\s+")
    val myWinningNumbers                                      = cardNumbers.filter(winningNumbers.contains(_))
    calculateScore(myWinningNumbers, 0)

  def determineCopies(line: String): Seq[Int] =
    val Array(card, numbers)                               = line.split(": ")
    val Array(_, cardNumber)                               = card.split("Card\\s+")
    val Array(winningNumbersAsString, cardNumbersAsString) = numbers.trim.split('|')
    val winningNumbers                                     = winningNumbersAsString.trim.split("\\s+")
    val cardNumbers                                        = cardNumbersAsString.trim.split("\\s+")
    val myWinningNumbers                                   = cardNumbers.filter(winningNumbers.contains(_))
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
}
