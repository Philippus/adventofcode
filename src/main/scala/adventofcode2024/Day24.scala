package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day24:
  enum Operation:
    def value(v1: Boolean, v2: Boolean): Boolean = this match
      case AND => v1 && v2
      case OR  => v1 || v2
      case XOR => v1 ^ v2
    case AND extends Operation
    case OR  extends Operation
    case XOR extends Operation

  object Operation:
    def fromString(s: String): Operation =
      s match
        case "AND" => AND
        case "OR"  => OR
        case "XOR" => XOR
  end Operation

  case class Wire(gate1: String, op: Operation, gate2: String)

  def simulateGates(values: Map[String, Boolean], wires: Seq[(Wire, String)]): BigInt =
    val map: scala.collection.mutable.Map[String, Boolean] = scala.collection.mutable.Map[String, Boolean]()
    values.foreach(value => map += value)
    while wires.exists(w => !map.contains(w._2)) do
      wires.foreach:
        case (wire, value) if map.contains(wire.gate1) && map.contains(wire.gate2) && !map.contains(value) =>
          map += value -> wire.op.value(map(wire.gate1), map(wire.gate2))
        case (wire, value)                                                                                 =>
          ()
    BigInt(map.filter(_._1.startsWith("z")).toSeq.sortBy(_._1).reverse.map(w => if w._2 then "1" else "0").mkString, 2)

  def handleLines(lines: List[String]): (Map[String, Boolean], Seq[(Wire, String)]) =
    val initialValues = lines.takeWhile(_.nonEmpty).map:
      case s"$w: $b" => w -> (b == "1") // x1: 1
    val wires         = lines.dropWhile(_.nonEmpty).filter(_.nonEmpty).map:
      case s"$w1 $op $w2 -> $w3" => Wire(w1, Operation.fromString(op), w2) -> w3 // x00 AND y00 -> z00
    (initialValues.toMap, wires)

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day24
