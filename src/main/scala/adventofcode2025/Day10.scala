package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import optimus.algebra.*
import optimus.optimization.*
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPIntVar

object Day10:
  case class Machine(
      goal: List[Boolean],
      wiring: List[List[Int]],
      joltageReqs: List[Int],
      current: List[Boolean],
      pushes: Long,
      counters: List[Int]
  ):
    def push(button: Int): Machine =
      if reachedGoal then
        this
      else
        val newCurrent = current.zipWithIndex.map: (a, b) =>
          if wiring(button).contains(b) then !current(b) else current(b)
        this.copy(current = newCurrent, pushes = this.pushes + 1)

    def pushForJoltage(button: Int): Machine =
      if reachedJoltageReqs then
        this
      else
        val newCounters = counters.zipWithIndex.map: (a, b) =>
          if wiring(button).contains(b) then counters(b) + 1 else counters(b)
        this.copy(counters = newCounters, pushes = this.pushes + 1)

    def reachedGoal: Boolean = current == goal

    def reachedJoltageReqs: Boolean = counters == joltageReqs

    def joltageCounterTooHigh: Boolean = counters.zip(joltageReqs).exists: (counter, joltageReq) =>
      counter > joltageReq
  end Machine

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

  def fewestButtonPressesForJoltageForMachine(machine: Machine): Long =
    @tailrec
    def loop(machines: List[Machine]): Long =
      if machines.exists(_.reachedJoltageReqs) then
        machines.find(_.reachedJoltageReqs).map(_.pushes).get
      else
        val pushedMachines =
          machines.flatMap: machine =>
            val newMachines = machine.wiring.zipWithIndex.map: (_, id) =>
              machine.pushForJoltage(id)
            newMachines
        loop(pushedMachines.filterNot(_.joltageCounterTooHigh).distinct)

    loop(List(machine))

  def fewestButtonPressesForJoltage(machines: List[Machine]): Long =
    // doesn't work fast enough for the input
    machines.map(fewestButtonPressesForJoltageForMachine).sum

  def fewestButtonPressesForJoltageWithMIPForMachine(machine: Machine): Long =
    given model: MPModel = MPModel(SolverLib.oJSolver)

    /* for [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    upper limits:
      b0 0..7
      b1 0..5
      b2 0..4
      b3 0..4
      b4 0..3
      b5 0..3
     */
    val mpIntVars = machine.wiring.zipWithIndex.map: (button, idx) =>
      val upperLimit = button.map: pos =>
        machine.joltageReqs(pos)
      .min
      MPIntVar(s"b$idx", 0 to upperLimit)

    minimize(mpIntVars.reduce(_ + _))

    /* for [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
                  b0   b1    b2   b3    b4    b5
    c0                                  x     x   3
    c1                 x                      x   5
    c2                       x    x     x         4
    c3            x    x          x               7

   expressions:
     c0 => a4 * b4 + a5 * b5 := 3
     c1 => a1 * b1 + a5 * b5 := 5
     c2 => a2 * b2 + a3 * b3 + a4 * b4 := 4
     c3 => a0 * b0 + a1 * b1 + a3 * b3 := 7
     */
    val constraints = machine.joltageReqs.zipWithIndex.map: (joltageReq, idx) =>
      machine.wiring.zipWithIndex.filter(_._1.contains(idx)).map: (_, buttonIdx) =>
        mpIntVars(buttonIdx)
      .reduce[Expression](_ + _) := joltageReq

    subjectTo(constraints*)

    start()
    release()

    objectiveValue.round

  def fewestButtonPressesForJoltageWithMIP(machines: List[Machine]): Long =
    machines.map(fewestButtonPressesForJoltageWithMIPForMachine).sum

  def parse(input: String): List[Machine] =
    input.split('\n').toList.map:
      case s"[$gs] $ws {$js}" =>
        val goal        = gs.toList.map: light =>
          light == '#'
        val wiring      = ws.split(" ").toList.map:
          case s"($ww)" => ww.split(",").toList.map: www =>
              www.toInt
        val joltageReqs = js.split(",").toList.map: j =>
          j.toInt
        Machine(goal, wiring, joltageReqs, goal.map(_ => false), 0L, joltageReqs.map(_ => 0))

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day10input.txt")): source =>
      source.mkString
end Day10
