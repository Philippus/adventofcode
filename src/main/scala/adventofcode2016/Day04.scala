package adventofcode2016

import scala.io.Source
import scala.util.Using

object Day04:
  def sectorIdOfRealRoom(s: String): Int =
    s match
      case s"$firstpart[$checksum]" =>
        val split    = firstpart.split('-')
        val letters  = split.init.mkString
        val sectorId = split.last.toInt
        if checksum ==
            letters.groupBy(identity).toSeq.sortBy((c, cs) => (-cs.length, c)).take(5).map(_._1).mkString
        then
          sectorId
        else
          0

  def decryptEncryptedRoomName(s: String): String =
    val chars = "abcdefghijklmnopqrstuvwxyz"
    s match
      case s"$firstpart[$checksum]" =>
        val split    = firstpart.split('-')
        val letters  = split.init.mkString("-")
        val sectorId = split.last.toInt
        letters.map: letter =>
          if letter == '-' then ' '
          else
            chars((chars.indexOf(letter) + sectorId) % 26)

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day04input.txt")): source =>
      source.getLines().toSeq
end Day04
