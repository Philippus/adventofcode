package adventofcode2015

import scala.annotation.tailrec

object Day11:
  val letters = "abcdefghijklmnopqrstuvwxyz"

  def isValid(password: String): Boolean =
    val repeated = letters.map(c => c.toString + c.toString).mkString
    val triples  = letters.sliding(3).toSeq
    val doubles  = repeated.grouped(2).toSeq
    triples.exists(password.contains) &&
    !Seq('i', 'o', 'l').exists(c => password.contains(c)) &&
    doubles.count(password.contains) == 2

  def nextChar(c: Char): Char =
    if (c == 'z') then 'a'
    else letters.charAt(letters.indexOf(c) + 1)

  @tailrec
  def rotateCharAt(password: String, index: Int): String =
    if password.charAt(index) == 'z' then
      rotateCharAt(password.updated(index, nextChar(password.charAt(index))), index - 1)
    else
      password.updated(index, nextChar(password.charAt(index)))

  @tailrec
  def nextPassword(password: String): String =
    val candidate = rotateCharAt(password, 7)
    if isValid(candidate) then
      candidate
    else
      nextPassword(candidate)
end Day11
