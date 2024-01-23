package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day06:
  def countAnswers(answers: String) =
    answers.split("\n\n").map(_.split("\n").mkString.distinct.length).sum

  def countQuestionsEveryoneAnswered(answers: String) =
    val answersPerGroup = answers.split("\n\n").map(_.split("\n").toSeq).toSeq
    val answeredByAll =
      for
        answersOfGroup <- answersPerGroup
        answeredByAllGrouped   = answersOfGroup.mkString.groupBy(identity).filter(_._2.length == answersOfGroup.length)
      yield answeredByAllGrouped.keys
    answeredByAll.map(_.size).sum

  def readInputFile(): String =
    Using.resource(Source.fromResource("2020/day06input.txt")): source =>
      source.mkString
end Day06
