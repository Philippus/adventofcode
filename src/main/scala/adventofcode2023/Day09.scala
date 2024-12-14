package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:
  @tailrec
  def sequenceOfDifferences(history: Seq[Int], acc: Seq[Seq[Int]] = Seq.empty): Seq[Seq[Int]] =
    if history.forall(x => x == 0) then
      acc
    else
      val newHistory = history.sliding(2).toList.map:
        case a :: b :: _ => b - a
      if acc.isEmpty then
        sequenceOfDifferences(newHistory, Seq(history, newHistory))
      else
        sequenceOfDifferences(newHistory, acc :+ newHistory)

  @tailrec
  def predictNextValue(sequence: Seq[Seq[Int]]): Int =
    if sequence.length == 1 then
      sequence.head.last
    else
      val prediction = sequence.init.last.last + sequence.last.last
      predictNextValue(sequence.init.init :+ (sequence.init.last :+ prediction))

  @tailrec
  def predictPreviousValue(sequence: Seq[Seq[Int]]): Int =
    if sequence.length == 1 then
      sequence.head.head
    else
      val prediction = sequence.init.last.head - sequence.last.head
      predictPreviousValue(sequence.init.init :+ (prediction +: sequence.init.last))

  def sumExtrapolatedNextValues(histories: Seq[Seq[Int]]): Int =
    histories.map(sequenceOfDifferences(_)).map(predictNextValue).sum

  def sumExtrapolatedPreviousValues(histories: Seq[Seq[Int]]): Int =
    histories.map(sequenceOfDifferences(_)).map(predictPreviousValue).sum

  def importLines(): List[List[Int]] =
    Using.resource(Source.fromResource(s"2023/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList.map(_.split(' ').toList.map(_.toInt))
end Day09
