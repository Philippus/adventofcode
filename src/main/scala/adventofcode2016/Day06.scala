package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06:
  def handleMessages(messages: Seq[String]): String =
    @tailrec
    def loop(pos: Int, acc: String): String =
      if pos == messages.head.length then
        acc
      else
        val char = messages.map(m => m(pos)).groupBy(identity).toSeq.maxBy(_._2.size)._1
        loop(pos + 1, acc :+ char)

    loop(0, "")

  def handleMessagesPartTwo(messages: Seq[String]): String =
    @tailrec
    def loop(pos: Int, acc: String): String =
      if pos == messages.head.length then
        acc
      else
        val char = messages.map(m => m(pos)).groupBy(identity).toSeq.minBy(_._2.size)._1
        loop(pos + 1, acc :+ char)

    loop(0, "")
  def readInputfile(): Seq[String]                         =
    Using.resource(Source.fromResource("2016/day06input.txt")): source =>
      source.getLines().toSeq
end Day06
