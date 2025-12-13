package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day25:
  def findLoopSize(publicKey: Long): Long =
    val subjectNumber = 7L
    var value         = 1L
    var loops         = 0L
    while value != publicKey do
      value = value * subjectNumber % 20201227
      loops += 1L
    loops

  def findEncryptionKey(publicKey: Long, loopSize: Long): Long =
    var value = 1L
    var loops = 0
    while loops != loopSize do
      value = value * publicKey % 20201227
      loops += 1
    value

  def parse(input: String): (Long, Long) =
    val keys = input.split('\n').toList.map(_.toLong)
    (keys.head, keys.last)

  def importLines(): String =
    Using.resource(Source.fromResource("2020/day25input.txt")): source =>
      source.mkString
end Day25
