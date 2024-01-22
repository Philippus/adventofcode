package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day04:
  def validatePassport(s: String): Boolean =
    s.contains("byr") &&
      s.contains("iyr") &&
      s.contains("eyr") &&
      s.contains("hgt") &&
      s.contains("hcl") &&
      s.contains("ecl") &&
      s.contains("pid")

  def validatePassportPartTwo(s: String): Boolean =
    val wSpaces = s.split("\\s").mkString(" ") ++ " "
    val byr     = wSpaces match
      case s"${a}byr:$value $b" => value.toIntOption.exists(value => value >= 1920 && value <= 2002)
      case _                    => false
    val iyr     = wSpaces match
      case s"${a}iyr:$value $b" => value.toIntOption.exists(value => value >= 2010 && value <= 2020)
      case _                    => false
    val eyr     = wSpaces match
      case s"${a}eyr:$value $b" => value.toIntOption.exists(value => value >= 2020 && value <= 2030)
      case _                    => false
    val hgt     = wSpaces match
      case s"${a}hgt:${value}cm $b" => value.toIntOption.exists(value => value >= 150 && value <= 193)
      case s"${a}hgt:${value}in $b" => value.toIntOption.exists(value => value >= 59 && value <= 76)
      case _                        => false
    val hcl     = wSpaces match
      case s"${a}hcl:#${value} $b" => value.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
      case _                       => false
    val ecl     = wSpaces match
      case s"${a}ecl:${value} $b" => "amb blu brn gry grn hzl oth".split(" ").contains(value)
      case _                      => false
    val pid     = wSpaces match
      case s"${a}pid:${value} $b" => value.forall(_.isDigit) && value.length == 9
      case _                      => false
    byr && iyr && eyr && hgt && hcl && ecl && pid

  def splitIntoPassports(s: String): Seq[String] =
    s.split("\n\n").map(_.split("\n").mkString(" "))
  def readInputFile(): String                    =
    Using.resource(Source.fromResource("2020/day04input.txt")): source =>
      source.mkString
end Day04
