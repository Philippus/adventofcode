package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day15:
  def generate(i: Long, factor: Long): Long = i * factor % 2147483647

  def generateWithMultiple(i: Long, factor: Long, multipleOf: Long): Long =
    val newVal = i * factor % 2147483647
    if newVal % multipleOf == 0 then newVal
    else generateWithMultiple(newVal, factor, multipleOf)

  def compare(i: Long, j: Long): Boolean =
    i.toBinaryString.takeRight(16) == j.toBinaryString.takeRight(16)

  def judge(generatorA: Long, factorA: Long, generatorB: Long, factorB: Long, limit: Long): Long =
    def loop(a: Long, b: Long, compared: Long, acc: Long): Long =
      if compared > limit then
        acc
      else
        val newA = generate(a, factorA)
        val newB = generate(b, factorB)
        if compare(newA, newB) then
          loop(newA, newB, compared + 1, acc + 1)
        else
          loop(newA, newB, compared + 1, acc)

    loop(generatorA, generatorB, 0, 0)

  def judgeWithMultiple(
      generatorA: Long,
      factorA: Long,
      multipleA: Long,
      generatorB: Long,
      factorB: Long,
      multipleB: Long,
      limit: Long
  ): Long =
    def loop(a: Long, b: Long, compared: Long, acc: Long): Long =
      if compared > limit then
        acc
      else
        val newA = generateWithMultiple(a, factorA, multipleA)
        val newB = generateWithMultiple(b, factorB, multipleB)
        if compare(newA, newB) then
          loop(newA, newB, compared + 1, acc + 1)
        else
          loop(newA, newB, compared + 1, acc)

    loop(generatorA, generatorB, 0, 0)
end Day15
