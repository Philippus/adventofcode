package adventofcode2016

import java.security.MessageDigest
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day14:
  val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")

  def md5(s: String): String =
    val hash = messageDigest.digest(s.getBytes)
    val hex  = new StringBuilder()
    for (b <- hash)
      hex.append(String.format("%02x", b))
    hex.toString

  def stretchedHash(s: String): String =
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

  def determineIndexOf64thKey(line: String, stretched: Boolean = false): Int =
    val map = mutable.SortedMap[String, String]()

    def md5Memo(s: String): String = map.getOrElseUpdate(s, md5(s))

    def stretchedHashMemo(s: String): String = map.getOrElseUpdate(s, stretchedHash(s))

    var i: Int       = 0
    var indexesFound = 0
    while (indexesFound < 64)
      hasTriple(if stretched then stretchedHashMemo(s"$line$i") else md5Memo(s"$line$i")) match
        case Some(c) =>
          var j          = i + 1
          var indexFound = false
          while (j - i < 1001 && !indexFound)
            if (hasQuintuple(if stretched then stretchedHashMemo(s"$line$j") else md5Memo(s"$line$j"), c))
              indexFound = true
              indexesFound += 1
            j += 1
        case None    =>
      i += 1
    i - 1

  def importLines(): String =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next()
end Day14
