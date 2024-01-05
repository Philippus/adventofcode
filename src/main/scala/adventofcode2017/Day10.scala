package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  def processLengths(numbers: Seq[Int], lengths: Seq[Int]) =
    def loop(numbers: Seq[Int], lengths: Seq[Int], currentPosition: Int, skipSize: Int): Int =
      if lengths.isEmpty then
        numbers.head * numbers.tail.head
      else
        val length          = lengths.head
        val (first, second) = numbers.splitAt(currentPosition)
        val workOn          = (second ++ first).take(length)
        val dontWorkOn      = (second ++ first).drop(length)
        val workOnReversed  = workOn.reverse
        val a               = workOnReversed ++ dontWorkOn
        val newNumbers      = a.takeRight(currentPosition) ++ a.dropRight(currentPosition)
        loop(newNumbers, lengths.tail, (currentPosition + length + skipSize) % numbers.length, skipSize + 1)
    loop(numbers, lengths, 0, 0)

  val suffix                                        = Seq(17, 31, 73, 47, 23)
  def stringToLengthsAndSuffix(s: String): Seq[Int] =
    s.map(_.toInt) ++ suffix

  def reduceSparseHash(numbers: Seq[Int]): Seq[Int] =
    numbers.grouped(16).map(group => group.foldLeft(0)((a, b) => a ^ b)).toSeq

  def numbersToHex(numbers: Seq[Int]): String =
    numbers.map(i =>
      val j = i.toHexString
      if j.length == 1 then
        "0" ++ j
      else
        j
    ).mkString

  def processLengthsAsAscii(initialNumbers: Seq[Int], initialLengths: Seq[Int]) =
    def loop(numbers: Seq[Int], lengths: Seq[Int], currentPosition: Int, skipSize: Int, rounds: Int): String =
      if lengths.isEmpty && rounds == 64 then
        numbersToHex(reduceSparseHash(numbers))
      else if lengths.isEmpty then
        loop(numbers, initialLengths, currentPosition, skipSize, rounds + 1)
      else
        val length          = lengths.head
        val (first, second) = numbers.splitAt(currentPosition)
        val workOn          = (second ++ first).take(length)
        val dontWorkOn      = (second ++ first).drop(length)
        val workOnReversed  = workOn.reverse
        val a               = workOnReversed ++ dontWorkOn
        val newNumbers      = a.takeRight(currentPosition) ++ a.dropRight(currentPosition)
        loop(newNumbers, lengths.tail, (currentPosition + length + skipSize) % numbers.length, skipSize + 1, rounds)
    loop(initialNumbers, initialLengths, 0, 0, 1)

  def readInputFileAsAscii(): Seq[Int] =
    Using.resource(Source.fromResource("2017/day10input.txt")):
      _.getLines().toSeq.head.map(_.toInt)

  /** The list begins as [0] 1 2 3 4 (where square brackets indicate the current position). The first length, 3, selects
    * ([0] 1 2) 3 4 (where parentheses indicate the sublist to be reversed). After reversing that section (0 1 2 into 2
    * 1 0), we get ([2] 1 0) 3 4. Then, the current position moves forward by the length, 3, plus the skip size, 0: 2 1
    * 0 [3] 4. Finally, the skip size increases to 1.
    *
    * @return
    */
  def readInputFile(): Seq[Int] =
    Using.resource(Source.fromResource("2017/day10input.txt")):
      _.getLines().toSeq.head.split(',').map(_.toInt)
end Day10
