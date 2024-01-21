package adventofcode2016

object Day16:
  def dragonCurve(s: String, length: Int): String =
    var data = s
    while (data.length < length)
      val a = data
      val b = a.reverse.map:
        case '0' => '1'
        case '1' => '0'
      data = s"${a}0${b}"
    data

  def generateChecksum(s: String): String =
    var data = s
    var done = false
    while (!done)
      data = data.grouped(2).map(group => if group.head == group.last then "1" else "0").mkString
      if data.length % 2 == 1 then
        done = true
    data

  def fillDisk(s: String, length: Int): String =
    val curve = dragonCurve(s, length)
    generateChecksum(curve.take(length))
end Day16
