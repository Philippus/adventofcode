package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day21:
  val weapons = Map(
    "Dagger"     -> Item(8, 4, 0),
    "Shortsword" -> Item(10, 5, 0),
    "Warhammer"  -> Item(25, 6, 0),
    "Longsword"  -> Item(40, 7, 0),
    "Greataxe"   -> Item(74, 8, 0)
  )
  val armory  = Map(
    "Naked"      -> Item(0, 0, 0),
    "Leather"    -> Item(13, 0, 1),
    "Chainmail"  -> Item(31, 0, 2),
    "Splintmail" -> Item(53, 0, 3),
    "Bandedmail" -> Item(75, 0, 4),
    "Platemail"  -> Item(102, 0, 5)
  )
  val rings   = Map(
    "NothingL"   -> Item(0, 0, 0),
    "NothingR"   -> Item(0, 0, 0),
    "Damage +1"  -> Item(25, 1, 0),
    "Damage +2"  -> Item(50, 2, 0),
    "Damage +3"  -> Item(100, 3, 0),
    "Defense +1" -> Item(20, 0, 1),
    "Defense +2" -> Item(40, 0, 2),
    "Defense +3" -> Item(80, 0, 3)
  )
  case class Item(cost: Int, damage: Int, armor: Int)

  sealed trait Combatant
  final case class Boss(hitPoints: Int, damage: Int, armor: Int) extends Combatant

  final case class Player(hitPoints: Int, damage: Int, armor: Int) extends Combatant

  @tailrec
  def battle(player: Player, boss: Boss, turn: Combatant): Combatant =
    if player.hitPoints <= 0 then boss
    else if boss.hitPoints <= 0 then player
    else
      turn match
        case Player(_, _, _) =>
          val damageDealt   = scala.math.max(1, player.damage - boss.armor)
          val hitpointsLeft = scala.math.max(0, boss.hitPoints - damageDealt)
//          println(s"- The player deals ${player.damage}-${boss.armor} = ${damageDealt} damage; the boss goes down to ${hitpointsLeft} hit points.")
          battle(player, boss.copy(hitPoints = hitpointsLeft), boss)
        case Boss(_, _, _)   =>
          val damageDealt   = scala.math.max(1, boss.damage - player.armor)
          val hitpointsLeft = scala.math.max(0, player.hitPoints - damageDealt)
//          println(s"- The boss deals ${boss.damage}-${player.armor} = ${damageDealt} damage; the player goes down to ${hitpointsLeft} hit points.")
          battle(player.copy(hitPoints = hitpointsLeft), boss, player)

  def calculateMinimumSpend(boss: Boss): Int =
    val possibleOutfits =
      for
        weapon <- weapons
        armor  <- armory
        ringL  <- rings
        ringR  <- rings.removed(ringL._1)
      yield Seq(weapon, armor, ringL, ringR)

    val playersToCost = possibleOutfits.map: outfit =>
      val damage = outfit.map(_._2.damage).sum
      val armor  = outfit.map(_._2.armor).sum
      (Player(100, damage, armor), outfit.map(_._2.cost).sum)

    playersToCost.map(pToC => (battle(pToC._1, boss, pToC._1), pToC._2)).filter(x =>
      x._1 match {
        case _: Player => true
        case _: Boss   => false
      }
    ).map(
      _._2
    ).min

  def calculateMaximumSpend(boss: Boss): Int =
    val possibleOutfits =
      for
        weapon <- weapons
        armor  <- armory
        ringL  <- rings
        ringR  <- rings.removed(ringL._1)
      yield Seq(weapon, armor, ringL, ringR)

    val playersToCost = possibleOutfits.map: outfit =>
      val damage = outfit.map(_._2.damage).sum
      val armor  = outfit.map(_._2.armor).sum
      (Player(100, damage, armor), outfit.map(_._2.cost).sum)

    playersToCost.map(pToC => (battle(pToC._1, boss, pToC._1), pToC._2)).filter(x =>
      x._1 match {
        case _: Boss   => true
        case _: Player => false
      }
    ).map(
      _._2
    ).max
end Day21
