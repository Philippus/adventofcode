package adventofcode2017

import scala.io.Source
import scala.util.Using

object Day04:
  def validate(passphrase: String): Boolean =
    val splits = passphrase.split("\\s")
    splits.distinct.length == splits.length

  def validateWithNewPolicy(passphrase: String): Boolean =
    val splits = passphrase.split("\\s")
    splits.map(x => x.sorted).distinct.length == splits.length

  def readInputFile: Seq[String] =
    Using.resource(Source.fromResource("2017/day04input.txt")):
      _.getLines().toSeq
end Day04
