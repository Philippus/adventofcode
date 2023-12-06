package adventofcode2015

import scala.annotation.tailrec

object Day10:
  def nextSequence(sequence: String): String =
    @tailrec
    def helper(sequence: String, current: String, acc: String): String =
      sequence match
        case s if s.isEmpty => acc ++ s"${current.length.toString}${current.head.toString}"
        case s if current.isEmpty || current.lastOption.contains(s.head) => helper(sequence.tail, current + s.head, acc)
        case s => helper(sequence.tail, s.head.toString, acc ++ s"${current.length.toString}${current.head.toString}")

    helper(sequence, "", "")

  @tailrec
  def nthSequence(sequence: String, n: Int): String =
    if n == 0 then sequence
    else nthSequence(nextSequence(sequence), n - 1)
end Day10
