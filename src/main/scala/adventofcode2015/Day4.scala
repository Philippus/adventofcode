package adventofcode2015

import java.security.MessageDigest

object Day4:
  val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")

  def md5(s: String): String =
    val hash = messageDigest.digest(s.getBytes)
    val hex = new StringBuilder()
    for (b <- hash)
      hex.append(String.format("%02x", b))
    hex.toString

  def determineSecretKey(line: String, amountOfZeroes: Int): Int =
    var i: Int = 1
    val zeroes = "0" * amountOfZeroes
    while (!md5(s"$line$i").startsWith(zeroes))
      i += 1
    i
end Day4
