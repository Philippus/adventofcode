package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day13:
  def handleLine(line: String): (String, Int, String) =
    line match
      case s"$a would gain $b happiness units by sitting next to $c." =>
        (a, b.toInt, c)
      case s"$a would lose $b happiness units by sitting next to $c." =>
        (a, -b.toInt, c)

  def calculateHappiness(seatingArrangement: List[String], values: Seq[(String, Int, String)]): Int =
    @tailrec
    def helper(seatingArrangement: List[String], acc: Int): Int =
      seatingArrangement match
        case Nil =>
          acc
        case b :: Nil =>
          acc
        case a :: b :: rest =>
          val add = values.find(value => value._1 == a && value._3 == b).map(_._2).get +
            values.find(value => value._3 == a && value._1 == b).map(_._2).get
          helper(b :: rest, acc + add)
    helper(seatingArrangement :+ seatingArrangement.head, 0)

  def addAttendee(attendees: Set[String], extraAttendee: String): Seq[(String, Int, String)] =
    attendees.toSeq.flatMap(attendee => Seq((extraAttendee, 0, attendee), (attendee, 0, extraAttendee)))

  def maximumHappinessForInputFile(extraAttendee: Option[String]): Int =
    Using.resource(Source.fromResource("2015/day13input.txt")): source =>
      val initialValues = source.getLines().map(handleLine).toSeq
      val attendees = initialValues.map(_._1).toSet
      val completeValues = extraAttendee match
        case Some(value) => initialValues ++ addAttendee(attendees, value)
        case None => initialValues
      val seatingArrangements = (extraAttendee match
        case Some(value) => (attendees + value)
        case None => (attendees)).toList.permutations
      seatingArrangements.map(seatingArrangement => calculateHappiness(seatingArrangement, completeValues)).max
end Day13
