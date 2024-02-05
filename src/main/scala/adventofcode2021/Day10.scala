package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  def syntaxErrorScore(s: String): Int =
    @tailrec
    def loop(s: String, acc: String): Int =
      if s.isEmpty then
        0
      else
        s.head match
          case '('                    => loop(s.tail, acc :+ '(')
          case '['                    => loop(s.tail, acc :+ '[')
          case '{'                    => loop(s.tail, acc :+ '{')
          case '<'                    => loop(s.tail, acc :+ '<')
          case ')' if acc.last == '(' => loop(s.tail, acc.init)
          case ')'                    => 3
          case ']' if acc.last == '[' => loop(s.tail, acc.init)
          case ']'                    => 57
          case '}' if acc.last == '{' => loop(s.tail, acc.init)
          case '}'                    => 1197
          case '>' if acc.last == '<' => loop(s.tail, acc.init)
          case '>'                    => 25137
    loop(s, "")

  def syntaxErrorScoreForSeq(s: Seq[String]): Int =
    s.map(syntaxErrorScore).sum

  def scoreCompletionString(s: String): Long =
    @tailrec
    def loop(s: String, acc: Long): Long =
      if s.isEmpty then
        acc
      else
        s.head match
          case ')' => loop(s.tail, acc * 5 + 1)
          case ']' => loop(s.tail, acc * 5 + 2)
          case '}' => loop(s.tail, acc * 5 + 3)
          case '>' => loop(s.tail, acc * 5 + 4)

    loop(s, 0)

  def completionString(s: String): String =
    s.reverse.map {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
    }

  def scoreIncompleteLine(s: String): Long =
    @tailrec
    def loop(s: String, acc: String): Long =
      if s.isEmpty then
        scoreCompletionString(completionString(acc))
      else
        s.head match
          case '('                    => loop(s.tail, acc :+ '(')
          case '['                    => loop(s.tail, acc :+ '[')
          case '{'                    => loop(s.tail, acc :+ '{')
          case '<'                    => loop(s.tail, acc :+ '<')
          case ')' if acc.last == '(' => loop(s.tail, acc.init)
          case ')'                    => -1
          case ']' if acc.last == '[' => loop(s.tail, acc.init)
          case ']'                    => -1
          case '}' if acc.last == '{' => loop(s.tail, acc.init)
          case '}'                    => -1
          case '>' if acc.last == '<' => loop(s.tail, acc.init)
          case '>'                    => -1
    loop(s, "")

  def middleScoreOfIncompleteLines(s: Seq[String]): Long =
    val sortedScores = s.map(scoreIncompleteLine).filter(_.>=(0)).sorted
    sortedScores.drop(sortedScores.length / 2).head

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2021/day10input.txt")): source =>
      source.getLines().toSeq
end Day10
