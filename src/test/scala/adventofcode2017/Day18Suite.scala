package adventofcode2017

import org.apache.pekko.actor.typed.ActorSystem
import adventofcode2017.Day18.*
import munit.FunSuite

class Day18Suite extends FunSuite:
  test("follows instructions of the example"):
    val instructions = Seq(
      "set a 1",
      "add a 2",
      "mul a a",
      "mod a 5",
      "snd a",
      "set a 0",
      "rcv a",
      "jgz a -1",
      "set a 1",
      "jgz a -2"
    )
    assertEquals(followInstructions(instructions), 4L)

  test("follows instructions of the input"):
    assertEquals(followInstructions(readInputFile()), 7071L)

  test("computes until deadlock for the sample"):
    val program                                       = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d".split('\n').toSeq
    val system: ActorSystem[HelloWorldMain.Boot.type] = ActorSystem(HelloWorldMain(program), "hello")
    system ! HelloWorldMain.Boot
    // actor system is killed if both programs are in state "waiting":
    // program 0, state waiting, count is 3
    // program 1, state waiting, count is 3
    Thread.sleep(3000)

  test("computes until deadlock for the input"):
    val system: ActorSystem[HelloWorldMain.Boot.type] = ActorSystem(HelloWorldMain(readInputFile()), "hello")
    system ! HelloWorldMain.Boot
    // actor system is killed if both programs are in state "waiting":
    // program 0, state waiting, count is 8128
    // program 1, state waiting, count is 8001
    Thread.sleep(3000)
end Day18Suite
