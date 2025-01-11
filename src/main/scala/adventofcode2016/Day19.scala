package adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object Day19:
  val buf = new ArrayBuffer[Int]

  @tailrec
  def loop(): Int =
    if buf.size == 1 then
      buf.head
    else
      val isOdd = buf.size % 2 == 1
      for
        i <- (buf.size - 1) to 0 by -1
        if i % 2 == 1
      do
        buf.remove(i)
      if isOdd then
        buf.insert(0, buf.last)
        buf.remove(buf.size - 1)
      loop()

  def stealPresents(elves: Int): Int =
    for
      i <- 1 to elves
    do
      buf += i
    loop()

  // https://en.wikipedia.org/wiki/Josephus_problem
  def getSafePosition(n: Int): Int =
    val valueOfL = n - Integer.highestOneBit(n);
    2 * valueOfL + 1;

  def stealPresentsDirectlyAcrossTheCircle(elves: Int): Int =
    val elvesVector = 1.to(elves).toVector
    var leftElves   = elvesVector.take(elvesVector.length / 2)
    var rightElves  = elvesVector.drop(elvesVector.length / 2)
    while leftElves.nonEmpty do
      rightElves = rightElves.drop(1)
      if leftElves.length == rightElves.length then // balance the "halves"
        leftElves = leftElves ++ rightElves.take(1)
        rightElves = rightElves.drop(1)
      rightElves = rightElves ++ leftElves.take(1)
      leftElves = leftElves.drop(1)
    rightElves.head

  def importLines(): Int =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.head.toInt
end Day19
