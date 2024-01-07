package adventofcode2017

import scala.collection.mutable.ArrayBuffer

object Day17:
  def insert(spins: Int, initialTimes: Int): ArrayBuffer[Int] =
    val state      = new ArrayBuffer[Int].addOne(0)
    var times      = 0
    var currentPos = 0
    while times != initialTimes do
      times = times + 1
      val newPos = (currentPos + spins) % times + 1
      state.insert(newPos, times)
      currentPos = newPos
    state

  def lastValueAfterZero(spins: Int, initialTimes: Int): Int =
    var times              = 0
    var currentPos         = 0
    var lastValueAfterZero = -1
    while times != initialTimes do
      times = times + 1
      val newPos = (currentPos + spins) % times + 1
      if newPos == 1 then lastValueAfterZero = times
      currentPos = newPos
    lastValueAfterZero
end Day17
