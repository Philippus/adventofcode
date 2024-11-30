package adventofcode2015

import adventofcode2015.Day18.*
import munit.FunSuite

class Day18Suite extends FunSuite:
  test("abc"):
    val currentState: Array[Array[Boolean]] =
      Array(
        Array(true, true, false, true, false, true),
        Array(false, false, false, true, true, false),
        Array(true, false, false, false, false, true),
        Array(false, false, true, false, false, false),
        Array(true, false, true, false, false, true),
        Array(true, true, true, true, false, true)
      )
//    println(currentState.map(_.toSeq).toSeq.mkString)
//    println(currentState.flatten.count(x => x))
//    println(nextState(currentState).flatten.count(x => x))
//    println(nextState(currentState).map(_.toSeq).toSeq.mkString)
    println(nextState(currentState).map(_.toSeq).toSeq.mkString)
    assert(true)
//  test("abc"):
//    val currentState: Array[Array[Boolean]] =
//      Array(
//        Array(false, true),
//        Array(false, true)
//      )
////    nextState(currentState)
//    //    println(currentState.map(_.toSeq).toSeq.mkString)
//    //    println(currentState.flatten.count(x => x))
//    //    println(nextState(currentState).flatten.count(x => x))
//    println(nextState(currentState).map(_.toSeq).toSeq.mkString)
//    //    println(nextState(nextState(nextState(nextState(currentState)))).map(_.toSeq).toSeq.mkString)
//    assert(true)

  test("abc"):
    importLines()
    val state =
      nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(nextState(grid))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

      assertEquals(state.flatten.count(x => x), 1)
end Day18Suite
