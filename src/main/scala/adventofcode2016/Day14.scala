package adventofcode2016

import java.security.MessageDigest

object Day14:
  val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")

  def md5(s: String): String =
    val hash = messageDigest.digest(s.getBytes)
    val hex  = new StringBuilder()
    for (b <- hash)
      hex.append(String.format("%02x", b))
    hex.toString

  def stretchedHash(s: String): String   =
    var i           = 0
    val initialHash = md5(s)
    var hash        = initialHash
    while (i < 2016)
      hash = md5(hash)
      i += 1
    hash
  def hasTriple(s: String): Option[Char] =
    s.sliding(3).find(_.distinct.length == 1).map(_.head)

  def hasQuintuple(s: String, c: Char): Boolean =
    s.sliding(5).exists(s => s.forall(_.==(c)))

  def determineIndexOf64thKey(line: String): Int =
    var i: Int       = 0
    var indexesFound = 0
    while (indexesFound < 64)
      hasTriple(md5(s"$line$i")) match
        case Some(c) =>
          var j          = i + 1
          var indexFound = false
          while (j - i < 1001 && !indexFound)
            if (hasQuintuple(md5(s"$line$j"), c))
              indexFound = true
              indexesFound += 1
            j += 1
        case None    =>
      i += 1
    i - 1

  def determineIndexOf64thKeyPartTwo(line: String): Int =
    var i: Int       = 0
    var indexesFound = 0
    while (indexesFound < 64)
      hasTriple(stretchedHash(s"$line$i")) match
        case Some(c) =>
          var j          = i + 1
          var indexFound = false
          while (j - i < 1001 && !indexFound)
            if (hasQuintuple(stretchedHash(s"$line$j"), c))
              indexFound = true
              indexesFound += 1
              println(s"$indexesFound for $i")
            j += 1
        case None    =>
      i += 1
    i - 1
end Day14
