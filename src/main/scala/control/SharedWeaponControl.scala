package control

import analyzers.ExplosionAnalyzer
import utils.{Bot, MiniBot, XY}

/**
 * Shared weapon control functions
 */
object SharedWeaponControl {

  val ExplosionThreshold = 1.20
  val RequiredVisibleEnemies = 1


  /**
   * Checks if enemies are too close and
   * that self destruct would be a good option.
   */
  def shouldSelfDestruct(bot: MiniBot): Boolean = {
    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // Too close, self destruct!
        if (delta.length <= 2) {
          return true
        }
      case None =>
    }

    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // Too close, self destruct!
        if (delta.length <= 2) {
          return true
        }
      case None =>
    }

    if ((bot.energy < 30 && bot.energy > 1) || bot.apocalypse < 10) {
      return true
    }

    false
  }

  /**
   * Finds most optimal blast radius and self-destructs.
   */
  def selfDestruct(bot: MiniBot) {
    val radiusAndDamage = ExplosionAnalyzer.apply(bot, bot.energy)
    //bot.say("DIE[" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString  + "]")
    bot.explode(radiusAndDamage._1)

  }

  /**
   * Analyzes if it is valuable to explode now.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryValuableExplosion(bot: MiniBot) = {
    var threshold = ExplosionThreshold
    if (bot.apocalypse < 500) threshold = ExplosionThreshold / 2
    else if (bot.apocalypse < 20) threshold = ExplosionThreshold / 10

    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot, bot.energy)
      if (radiusAndDamage._2 > (bot.energy * ExplosionThreshold)) {
        //bot.say("BOOM[" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString  + "]")
        //bot.say("KA-BOOM!")
        bot.explode(radiusAndDamage._1)
        true
      }
    }
    false
  }

  /**
   * Analyzes if it is valuable launch a drop bomb.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryDropBomb(bot: MiniBot) = {
    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies && bot.energy > 200) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot, 100)
      if (radiusAndDamage._2 > (100 * ExplosionThreshold)) {
        val relPos = bot.view.offsetToNearestEnemy()
        //bot.say("BOMB!")
        bot.spawn(relPos.signum, "type" -> "DropBomb")
        true
      }
    }
    false
  }

  /**
   * Checks if now is a good time to fire
   * a missile.
   */
  def checkFireMissile(bot: Bot): Boolean = {
    (bot.view.countType('m') > 0 || bot.view.countType('s') > 0 || bot.view.countType('b') > 3) &&
      bot.time > bot.inputAsIntOrElse("missileDelay", -1) &&
      bot.energy > 200
  }

  /**
   * Fire a missile.
   */
  def fireMissile(bot: Bot): Unit = {
    var fireRate = 3
    var power = 110
    if (bot.energy > 10000) {
      fireRate = 2
    }


    val relPos = bot.view.offsetToNearestEnemy()
    bot.spawn(relPos.signum, "type" -> "Missile", "target" -> relPos, "energy" -> (bot.energy / 5).min(400).max(100))
    if (bot.view.countType('m') > 0) {
      bot.set("missileDelay" -> -1)
    } else {
      bot.set("missileDelay" -> (bot.time + fireRate))
    }
  }

  /**
   * Spawn a Hunter
   */
  def spawnHunter(bot: Bot, moveDirection: XY): Unit = {
    bot.spawn(moveDirection.negate.signum, "type" -> "Hunter", "energy" -> 100)
    bot.say("Go now!")
  }

  /**
   * Spawn a Vampire
   */
  def spawnVampire(bot: Bot, moveDirection: XY): Unit = {
    bot.spawn(moveDirection.negate.signum, "type" -> "Vampire", "energy" -> (bot.energy / 10).min(1000).max(100))
    bot.say("Kill!")
  }

  /**
   * Check if in immediate danger.
   * If danger launch defence bot and return true,
   * else return false
   */
  def handleDanger(bot: Bot): Boolean = {
    val defenceTimeDelay = bot.inputAsIntOrElse("defenceDelay", 0)
    if (bot.time > defenceTimeDelay && bot.energy > 500 && bot.apocalypse > 50) {
      val slave = bot.view.offsetToNearest('s')
      slave match {
        case Some(pos: XY) =>
          if (pos.stepsTo(XY.Zero) <= 6) {
            //bot.say("Danger!")
            bot.spawn(pos.signum, "type" -> "Defence", "target" -> pos, "energy" -> (bot.energy / 40).min(500).max(100))
            if (bot.energy > 5000) {
              bot.set("defenceDelay" -> (bot.time + (5 - bot.view.countType('s'))))
            } else {
              bot.set("defenceDelay" -> (bot.time + (10 - bot.view.countType('s'))))
            }
            true
          } else {
            false
          }

        case None => false
      }
    }
    false
  }
}
