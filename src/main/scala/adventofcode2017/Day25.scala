package adventofcode2017

import scala.annotation.tailrec

object Day25:
  def followBlueprint(): Long =
    @tailrec
    def loop(state: Char, tape: Map[Long, Boolean], cursor: Long, steps: Int): Long =
      if steps == 6 then
        tape.count(_._2 == true).toLong
      else
        state match
          case 'A' if !tape.getOrElse(cursor, false) =>
            loop('B', tape + (cursor -> true), cursor + 1, steps + 1)
          case 'A' if tape.getOrElse(cursor, true)   =>
            loop('B', tape + (cursor -> false), cursor - 1, steps + 1)
          case 'B' if !tape.getOrElse(cursor, false) =>
            loop('A', tape + (cursor -> true), cursor - 1, steps + 1)
          case 'B' if tape.getOrElse(cursor, true)   =>
            loop('A', tape + (cursor -> true), cursor + 1, steps + 1)
    loop('A', Map.empty, 0, 0)

  def followBlueprintForInput(): Long =
    @tailrec
    def loop(state: Char, tape: Map[Long, Boolean], cursor: Long, steps: Int): Long =
      if steps == 12919244 then
        tape.count(_._2 == true).toLong
      else
        state match
          case 'A' if !tape.getOrElse(cursor, false) =>
            loop('B', tape + (cursor -> true), cursor + 1, steps + 1)
          case 'A' if tape.getOrElse(cursor, true)   =>
            loop('C', tape + (cursor -> false), cursor - 1, steps + 1)
          case 'B' if !tape.getOrElse(cursor, false) =>
            loop('A', tape + (cursor -> true), cursor - 1, steps + 1)
          case 'B' if tape.getOrElse(cursor, true)   =>
            loop('D', tape + (cursor -> true), cursor + 1, steps + 1)
          case 'C' if !tape.getOrElse(cursor, false) =>
            loop('A', tape + (cursor -> true), cursor + 1, steps + 1)
          case 'C' if tape.getOrElse(cursor, false)  =>
            loop('E', tape + (cursor -> false), cursor - 1, steps + 1)
          case 'D' if !tape.getOrElse(cursor, false) =>
            loop('A', tape + (cursor -> true), cursor + 1, steps + 1)
          case 'D' if tape.getOrElse(cursor, false)  =>
            loop('B', tape + (cursor -> false), cursor + 1, steps + 1)
          case 'E' if !tape.getOrElse(cursor, false) =>
            loop('F', tape + (cursor -> true), cursor - 1, steps + 1)
          case 'E' if tape.getOrElse(cursor, false)  =>
            loop('C', tape + (cursor -> true), cursor - 1, steps + 1)
          case 'F' if !tape.getOrElse(cursor, false) =>
            loop('D', tape + (cursor -> true), cursor + 1, steps + 1)
          case 'F' if tape.getOrElse(cursor, false)  =>
            loop('A', tape + (cursor -> true), cursor + 1, steps + 1)
    loop('A', Map.empty, 0, 0)
end Day25
