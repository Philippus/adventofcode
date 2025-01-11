package adventofcode2016

import java.security.MessageDigest
import scala.io.Source
import scala.util.Using

object Day17:
  val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")

  def md5(s: String): String =
    val hash = messageDigest.digest(s.getBytes)
    val hex  = new StringBuilder()
    for (b <- hash)
      hex.append(String.format("%02x", b))
    hex.toString

  def generateOptions(str: String): Seq[String] =
    val options = "UDLR"
    val open    = 'b'.to('f')
    val hash    = md5(str)
    0.to(3).collect:
      case i if open.contains(hash(i)) => str + options(i)
    .filter: str =>
      val rMinusL = str.count(_.==('R')) - str.count(_.==('L'))
      val dMinusU = str.count(_.==('D')) - str.count(_.==('U'))
      val rs      = str.count(_.==('R'))
      val ds      = str.count(_.==('D'))
      rMinusL >= 0 && rMinusL <= 3 && dMinusU >= 0 && dMinusU <= 3

  def pathToVault(initialStr: String, findMax: Boolean = false): Option[String] =
    def loop(str: String): Option[String] =
      val rMinusL = str.count(_.==('R')) - str.count(_.==('L'))
      val dMinusU = str.count(_.==('D')) - str.count(_.==('U'))
      if rMinusL == 3 && dMinusU == 3 then
        Some(str.drop(initialStr.length))
      else
        val options = generateOptions(str)
        if options.isEmpty then
          None
        else
          options.flatMap(loop).filterNot(_.isEmpty) match
            case Nil => None
            case seq => if findMax then Some(seq.maxBy(_.length)) else Some(seq.minBy(_.length))

    loop(initialStr)

  def lengthOfLongestPathToVault(initialStr: String): Option[Int] =
    pathToVault(initialStr, findMax = true).map(_.length)

  def importLines(): String =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next()
end Day17
