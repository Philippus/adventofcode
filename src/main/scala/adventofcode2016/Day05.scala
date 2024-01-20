package adventofcode2016

import java.security.MessageDigest

object Day05:
  val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")
  def md5(s: String): String       =
    val hash = messageDigest.digest(s.getBytes)
    val hex  = new StringBuilder()
    for (b <- hash)
      hex.append(String.format("%02x", b))
    hex.toString

  def determinePassword(line: String): String =
    var i: Int   = 1
    val zeroes   = "0" * 5
    var password = ""
    while (password.length < 8)
      if md5(s"$line$i").startsWith(zeroes) then
        password = password ++ md5(s"$line$i")(5).toString
      i += 1
    password

  def determinePasswordPartTwo(line: String): String =
    var i: Int   = 1
    val zeroes   = "0" * 5
    var password = "________"
    while (password.contains("_"))
      val md5Hash = md5(s"$line$i")
      if md5Hash.startsWith(zeroes) && md5Hash(5) >= '0' && md5Hash(5) <= '7' && password.charAt(
          md5Hash(5).asDigit
        ) == '_'
      then
        password = password.updated(md5Hash(5).asDigit, md5Hash(6))
        println(password)
      i += 1
    password
end Day05
