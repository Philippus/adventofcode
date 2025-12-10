package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  case class Machine(
      goal: List[Boolean],
      wiring: List[List[Int]],
      joltage: List[Int],
      current: List[Boolean],
      pushes: Long
  ):
    def push(button: Int): Machine =
      if reachedGoal then
        this
      else
        val newCurrent = current.zipWithIndex.map: (a, b) =>
          if wiring(button).contains(b) then !current(b) else current(b)
        Machine(goal, wiring, joltage, newCurrent, pushes + 1)

    def reachedGoal: Boolean = current == goal

  def fewestButtonPressesForMachine(machine: Machine): Long =
    @tailrec
    def loop(machines: List[Machine]): Long =
      if machines.exists(_.reachedGoal) then
        machines.find(_.reachedGoal).map(_.pushes).get
      else
        val pushedMachines =
          machines.flatMap: machine =>
            val newMachines = machine.wiring.zipWithIndex.map: (_, id) =>
              machine.push(id)
            newMachines
        loop(pushedMachines.distinct)

    loop(List(machine))

  def fewestButtonPresses(machines: List[Machine]): Long =
    machines.map(fewestButtonPressesForMachine).sum

  def parse(input: String): List[Machine] =
    input.split('\n').toList.map:
      case s"[$bs] $ws {$joltage}" =>
        val goal    = bs.toList.map: light =>
          light == '#'
        val wiring  = ws.split(" ").toList.map:
          case s"($ww)" => ww.split(",").toList.map: www =>
              www.toInt
        val joltage = List.empty[Int]
        Machine(goal, wiring, joltage, goal.map(_ => false), 0L)

  /*
  [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

   */
  def importLines(): String =
    Using.resource(Source.fromResource("2025/day10input.txt")): source =>
      source.mkString
end Day10
