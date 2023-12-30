package adventofcode2015

import adventofcode2015.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("processes the example battle"):
    val spellsSequences: Seq[Seq[Spell]] =
      Seq(Seq(Spell.Poison, Spell.MagicMissile))
    val minimumSpent: Int                = spellsSequences.map { spells =>
      battle(Player(10, 250, 0), Boss(13, 8), Player(10, 250, 0), Seq.empty, spells) match
        case Player(_, _, manaSpent) =>
          manaSpent
        case Boss(_, _, manaSpent)   =>
          manaSpent
    }.min
    assertEquals(minimumSpent, 226)

  test("determines the minimum mana for the example battle"):
    val spells         = Spell.values.toSeq
    val spellsSequence =
      for
        spell1 <- spells
        spell2 <- spells
        spell3 <- spells
        spell4 <- spells
      yield Seq(spell1, spell2, spell3, spell4)
    val minimumSpent   = spellsSequence.map { spells =>
      battle(Player(10, 250, 0), Boss(13, 8), Player(10, 250, 0), Seq.empty, spells) match
        case Player(_, _, manaSpent) =>
          manaSpent
        case Boss(_, _, manaSpent)   =>
          manaSpent
    }.min
    assertEquals(minimumSpent, 226)

  test("determines the minimum mana for the real battle"):
    val spells         = Spell.values
    val spellsSequence =
      for
        spell1 <- spells
        spell2 <- spells
        spell3 <- spells
        spell4 <- spells
        spell5 <- spells
        spell6 <- spells
        spell7 <- spells
        spell8 <- spells
        spell9 <- spells
      yield Seq(spell1, spell2, spell3, spell4, spell5, spell6, spell7, spell8, spell9)

    val minimumSpent = spellsSequence.map { spells =>
      battle(Player(50, 500, 0), Boss(58, 9), Player(50, 500, 0), Seq.empty, spells) match
        case Player(_, _, manaSpent) =>
          manaSpent
        case Boss(_, _, manaSpent)   =>
          manaSpent
    }.min
    assertEquals(minimumSpent, 1269)

  test("determines the minimum mana for the real battle in part two"):
    val spells         = Spell.values
    val spellsSequence =
      for
        spell1 <- spells
        spell2 <- spells
        spell3 <- spells
        spell4 <- spells
        spell5 <- spells
        spell6 <- spells
        spell7 <- spells
        spell8 <- spells
        spell9 <- spells
      yield Seq(spell1, spell2, spell3, spell4, spell5, spell6, spell7, spell8, spell9)

    val minimumSpent = spellsSequence.map { spells =>
      battle(Player(50, 500, 0), Boss(58, 9), Player(50, 500, 0), Seq.empty, spells, true) match
        case Player(_, _, manaSpent) =>
          manaSpent
        case Boss(_, _, manaSpent)   =>
          manaSpent
    }.min
    assertEquals(minimumSpent, 1309)
end Day22Suite
