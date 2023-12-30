package adventofcode2015

import scala.annotation.tailrec

object Day22:
  case class Effect(id: String, turns: Int, armor: Int, damage: Int, mana: Int)

  object Effect:
    val NoEffect: Effect = Effect("", 0, 0, 0, 0)
  end Effect

  enum Spell(val cost: Int, val damage: Int, val heals: Int, val effect: Effect):
    case MagicMissile extends Spell(53, 4, 0, Effect.NoEffect)
    case Drain        extends Spell(73, 2, 2, Effect.NoEffect)
    case Shield       extends Spell(113, 0, 0, Effect("Shield", 6, 7, 0, 0))
    case Poison       extends Spell(173, 0, 0, Effect("Poison", 6, 0, 3, 0))
    case Recharge     extends Spell(229, 0, 0, Effect("Recharge", 5, 0, 0, 101))

  sealed trait Combatant
  final case class Boss(hitPoints: Int, damage: Int, manaSpent: Int = Int.MaxValue) extends Combatant

  final case class Player(hitPoints: Int, mana: Int, manaSpent: Int) extends Combatant

  @tailrec
  def battle(
      player: Player,
      boss: Boss,
      turn: Combatant,
      activeEffects: Seq[Effect],
      nextSpells: Iterable[Spell],
      partTwo: Boolean = false
  ): Combatant =
    if player.hitPoints <= 0 then boss
    else if boss.hitPoints <= 0 then player
    else
      turn match
        case Player(_, _, _) =>
          if nextSpells.isEmpty then boss
          else
            val newPlayerHitPoints = player.hitPoints - (if partTwo then 1 else 0)
            if newPlayerHitPoints <= 0 then boss
            else
              val bossHitPointsAfterPoison = boss.hitPoints - activeEffects.map(_.damage).sum // poison
              if bossHitPointsAfterPoison <= 0 then player
              else
                val newPlayerMana    = player.mana + activeEffects.map(_.mana).sum // recharge
                val newActiveEffects =
                  activeEffects.map(effect => effect.copy(turns = effect.turns - 1)).filter(_.turns > 0)
                // cast spell
                val spell            = nextSpells.head
                if spell.cost > newPlayerMana || newActiveEffects.exists(_.id == spell.effect.id) then
                  boss // can't cast spell or already in effect
                else
                  battle(
                    player.copy(
                      hitPoints = newPlayerHitPoints + spell.heals,
                      mana = newPlayerMana - spell.cost,
                      manaSpent = player.manaSpent + spell.cost
                    ),
                    boss.copy(hitPoints = bossHitPointsAfterPoison - spell.damage),
                    boss,
                    (newActiveEffects :+ spell.effect).filter(_.turns > 0),
                    nextSpells.tail,
                    partTwo
                  )
        case Boss(_, _, _)   =>
          val newBossHitPoints = boss.hitPoints - activeEffects.map(_.damage).sum // poison
          if newBossHitPoints <= 0 then
            player
          else
            val playerArmor        = 0 + activeEffects.map(_.armor).sum          // shield
            val playerMana         = player.mana + activeEffects.map(_.mana).sum // recharge
            val newPlayerHitPoints = player.hitPoints - scala.math.max(1, boss.damage - playerArmor)
            val newActiveEffects   =
              activeEffects.map(effect => effect.copy(turns = effect.turns - 1)).filter(_.turns > 0)
            battle(
              player.copy(hitPoints = newPlayerHitPoints, mana = playerMana),
              boss.copy(hitPoints = newBossHitPoints),
              player,
              newActiveEffects,
              nextSpells,
              partTwo
            )
end Day22
