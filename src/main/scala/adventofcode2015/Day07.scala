package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Try, Using}

object Day07:
  case class Wire(id: String, signal: Int)

  @tailrec
  def runConnection(connections: Seq[String], wires: Set[Wire] = Set.empty[Wire]): Set[Wire] =
    if connections.isEmpty then
      wires
    else
      val connection = connections.head
      connection match
        case s"$x AND $y -> $d" if x.toIntOption.nonEmpty =>
          wires.find(_.id == y) match
            case Some(wire) =>
              runConnection(connections.tail, wires + Wire(d, x.toInt & wire.signal))
            case _          =>
              runConnection(connections.tail :+ connection, wires)
        case s"$x AND $y -> $d"                           =>
          (wires.find(_.id == x), wires.find(_.id == y)) match
            case (Some(wirex), Some(wirey)) =>
              runConnection(connections.tail, wires + Wire(d, wirex.signal & wirey.signal))
            case _                          =>
              runConnection(connections.tail :+ connection, wires)
        case s"$x OR $y -> $e" if x.toIntOption.nonEmpty  =>
          wires.find(_.id == y) match
            case Some(wire) =>
              runConnection(connections.tail, wires + Wire(e, x.toInt | wire.signal))
            case _          =>
              runConnection(connections.tail :+ connection, wires)
        case s"$x OR $y -> $e"                            =>
          (wires.find(_.id == x), wires.find(_.id == y)) match
            case (Some(wirex), Some(wirey)) =>
              runConnection(connections.tail, wires + Wire(e, wirex.signal | wirey.signal))
            case _                          =>
              runConnection(connections.tail :+ connection, wires)
        case s"$x LSHIFT $i -> $e"                        =>
          wires.find(_.id == x) match
            case Some(wirex) =>
              runConnection(connections.tail, wires + Wire(e, wirex.signal << i.toInt))
            case _           =>
              runConnection(connections.tail :+ connection, wires)
        case s"$x RSHIFT $i -> $e"                        =>
          wires.find(_.id == x) match
            case Some(wirex) =>
              runConnection(connections.tail, wires + Wire(e, wirex.signal >> i.toInt))
            case _           =>
              runConnection(connections.tail :+ connection, wires)
        case s"NOT $x -> $e"                              =>
          wires.find(_.id == x) match
            case Some(wirex) =>
              runConnection(connections.tail, wires + Wire(e, ~wirex.signal))
            case _           =>
              runConnection(connections.tail :+ connection, wires)
        case s"$x -> $w" if x.toIntOption.nonEmpty        =>
          runConnection(connections.tail, wires + Wire(w, x.toInt))
        case s"$x -> $w"                                  =>
          wires.find(_.id == x) match
            case Some(wirex) =>
              runConnection(connections.tail, wires + Wire(w, wirex.signal))
            case _           =>
              runConnection(connections.tail :+ connection, wires)
        case _                                            =>
          runConnection(connections.tail :+ connection, wires)

  def runCircuit: Int =
    Using.resource(Source.fromResource("2015/day07input.txt")): source =>
      val lines = source.getLines().toSeq
      runConnection(lines, Set.empty[Wire]).find(_.id == "a").get.signal

  def runCircuitAfterRewiring: Int =
    Using.resource(Source.fromResource("2015/day07input.txt")): source =>
      val lines = source.getLines().toSeq
      runConnection(lines, Set(Wire("b", runCircuit))).find(_.id == "a").get.signal
end Day07
