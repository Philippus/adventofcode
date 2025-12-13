package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day14:
  def execute(program: List[String]): Long =
    def decodeValue(value: Long, mask: String): Long =
      val valueAsBinaryString  = value.toBinaryString
      val widenedToMask        = "0".repeat(mask.length - valueAsBinaryString.length) ++ valueAsBinaryString
      val decodedValueAsString = widenedToMask.zip(mask).map: (f, m) =>
        (f, m) match {
          case (b, 'X') => b
          case (_, b)   => b
        }
      .mkString
      BigInt(decodedValueAsString, 2).longValue

    val memory = scala.collection.mutable.Map.empty[Int, Long]
    var mask   = ""
    program.foreach:
      case s"mask = $newMask"        =>
        mask = newMask
      case s"mem[$address] = $value" =>
        memory.update(address.toInt, decodeValue(value.toLong, mask))
    memory.values.sum

  def executeV2(program: List[String]): Long =
    def decodeAddress(address: Long, mask: String): List[Long] =
      val addressAsBinaryString  = address.toBinaryString
      val widenedToMask          = "0".repeat(mask.length - addressAsBinaryString.length) ++ addressAsBinaryString
      val decodedAddressAsString = widenedToMask.zip(mask).map: (f, m) =>
        (f, m) match {
          case (b, '0') => b
          case (b, '1') => '1'
          case (_, b)   => b
        }
      .mkString

      var allDecodedAddresses: List[String] = List(decodedAddressAsString)
      while allDecodedAddresses.exists(address => address.contains('X')) do
        val replacedXWith0 = allDecodedAddresses.map(address => address.replaceFirst("X", "0"))
        val replacedXWith1 = allDecodedAddresses.map(address => address.replaceFirst("X", "1"))
        allDecodedAddresses = replacedXWith0 ++ replacedXWith1
      allDecodedAddresses.map(address => BigInt(address, 2).longValue())

    val memory = scala.collection.mutable.Map.empty[Long, Long]
    var mask   = ""
    program.foreach:
      case s"mask = $newMask"        =>
        mask = newMask
      case s"mem[$address] = $value" =>
        decodeAddress(address.toLong, mask).foreach: address =>
          memory.update(address, value.toLong)
    memory.values.sum

  def parse(input: String): List[String] =
    input.split('\n').toList

  def importLines(): String =
    Using.resource(Source.fromResource("2020/day14input.txt")): source =>
      source.mkString
end Day14
