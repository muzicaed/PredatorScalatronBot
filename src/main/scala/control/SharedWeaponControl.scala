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
    val slaveToClose = bot.view.offsetToNearest('s') match {
      // Too close, self destruct!
      case Some(delta: XY) => delta.length <= 2
      case None => false
    }

    val masterToClose = bot.view.offsetToNearest('m') match {
      // Too close, self destruct!
      case Some(delta: XY) => delta.length < 2
      case None => false
    }

    if ((bot.energy < 30 && bot.energy > 1) || bot.apocalypse < 10) {
      return true
    }

    slaveToClose || masterToClose
  }

  /**
   * Finds most optimal blast radius and self-destructs.
   */
  def selfDestruct(bot: MiniBot) {
    val radiusAndDamage = ExplosionAnalyzer.apply(bot, bot.energy)
    bot.say("Goodbye![" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString + "]")
    bot.explode(radiusAndDamage._1)
  }

  /**
   * Analyzes if it is valuable to explode now.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryValuableExplosion(bot: MiniBot): Boolean = {
    var threshold = ExplosionThreshold
    if (bot.apocalypse < 500) threshold = ExplosionThreshold / 2
    else if (bot.apocalypse < 20) threshold = ExplosionThreshold / 10

    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot, bot.energy)
      if (radiusAndDamage._2 > (bot.energy * ExplosionThreshold)) {
        bot.say("BOOM[" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString + "]")
        bot.explode(radiusAndDamage._1)
        return true
      }
    }
    false
  }

  /**
   * Analyzes if it is valuable launch a drop bomb.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryDropBomb(bot: MiniBot): Boolean = {
    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies && bot.energy > 200) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot, 100)
      if (radiusAndDamage._2 > (100 * ExplosionThreshold)) {
        val relPos = bot.view.offsetToNearestEnemy()
        bot.say("BOMB!")
        bot.spawn(relPos.signum, "type" -> "DropBomb")
        return true
      }
    }
    false
  }

  /**
   * Checks if now is a good time to fire
   * a missile.
   */
  def checkFireMissile(bot: Bot): Boolean = {
    (bot.view.countType('m') > 0 || bot.view.countType('s') > 0 || bot.view.countType('b') > 2) &&
      bot.time > bot.inputAsIntOrElse("missileDelay", -1) &&
      bot.energy > 300
  }

  /**
   * Fire a missile.
   */
  def fireMissile(bot: Bot): Unit = {
    var fireRate = 4
    if (bot.energy > 10000) fireRate = 3
    val relPos = bot.view.offsetToNearestEnemy()

    bot.spawn(relPos.signum, "type" -> "Missile", "target" -> relPos.toDirection45, "energy" -> (bot.energy / 40).min(309).max(103))
    bot.set("missileDelay" -> (bot.time + fireRate))
  }

  /**
   * Spawn a Hunter
   */
  def spawnHunter(bot: Bot, direction: XY): Unit = {
    bot.spawn(direction.signum, "target" -> direction.toDirection45, "type" -> "Hunter", "energy" -> 105)
    bot.say("Go now!")
  }

  /**
   * Spawn a Vampire
   */
  def spawnVampire(bot: Bot, direction: XY): Unit = {
    bot.spawn(direction.signum, "target" -> direction.toDirection45, "type" -> "Vampire", "energy" -> (bot.energy / 10).min(1000).max(105))
    bot.say("Kill!")
  }

  /**
   * Check if in immediate danger.
   * If danger launch defence bot and return true,
   * else return false
   */
  def handleDanger(bot: Bot): Boolean = {
    val defenceTimeDelay = bot.inputAsIntOrElse("defenceDelay", 0)
    if (bot.time > defenceTimeDelay && bot.energy > 100) {
      val slave = bot.view.offsetToNearest('s')
      return slave match {
        case Some(pos: XY) =>
          if (pos.stepsTo(XY.Zero) <= 11) {
            bot.say("Danger!")
            bot.spawn(pos.signum, "type" -> "Defence", "target" -> pos.toDirection45, "energy" -> (bot.energy / 50).min(206).max(102))
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
