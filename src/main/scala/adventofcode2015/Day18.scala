package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day18:
  var grid: Array[Array[Boolean]] = Array.fill(100)(Array.fill(100)(false))

  def neighbours(state: Array[Array[Boolean]], x: Int, y: Int): Int =
    val coords =
      for
        xx <- (x - 1).to(x + 1)
        yy <- (y - 1).to(y + 1)
        if xx >= 0 && yy >= 0 && xx < state.length && yy < state.length && !(x == xx && y == yy)
      yield (xx, yy)
    coords.map((x, y) => state(x)(y)).count(_.==(true))

  def nextState(currentState: Array[Array[Boolean]]): Array[Array[Boolean]] =
    var workingState = Array.fill(currentState.length)(Array.fill(currentState.length)(false))
    val coords       =
      for
        x <- currentState.indices
        y <- currentState.indices
      yield (x, y)
    coords.foreach: (x, y) =>
      val n = neighbours(currentState, x, y)
      if (x == 0 && y == 0) || (x == currentState.length - 1 && y == 0) || (x == 0 && y == currentState
          .length - 1) || (x == currentState.length - 1 && y == currentState.length - 1)
      then workingState(x)(y) = true
      else if currentState(x)(y) && (n == 2 || n == 3) then workingState(x)(y) = true
      else if !currentState(x)(y) && n == 3 then workingState(x)(y) = true

    workingState

  def initializeGrid(lineWithIndex: (String, Int)): Unit =
    println(lineWithIndex)
    for
      x <- 0.until(lineWithIndex._1.length)
    yield if lineWithIndex._1.charAt(x) == '#' then grid(x)(lineWithIndex._2) = true
    grid(0)(0) = true
    grid(99)(99) = true
    grid(0)(99) = true
    grid(99)(0) = true

  def importLines(): Unit =
    Using.resource(Source.fromResource("2015/day18input.txt")): source =>
      source.getLines().toSeq.zipWithIndex.map(initializeGrid)
end Day18
