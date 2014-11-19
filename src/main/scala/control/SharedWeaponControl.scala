package control

import analyzers.ExplosionAnalyzer
import utils._

/**
 * Shared weapon control functions
 */
object SharedWeaponControl {

  /**
   * Checks if enemies are too close and
   * that self destruct would be a good option.
   */
  def shouldSelfDestruct(bot: MiniBot): Boolean = {
    val slaveToClose = bot.view.offsetToNearest(CellType.ENEMY_SLAVE) match {
      case Some(delta: XY) => delta.length <= 2
      case None => false
    }

    val masterToClose = bot.view.offsetToNearest(CellType.ENEMY_MASTER) match {
      case Some(delta: XY) => delta.length < 2
      case None => false
    }

    if ((bot.energy < 30 && bot.energy > 1) || bot.apocalypse < 5) {
      return true
    }

    return slaveToClose || masterToClose
    false
  }

  /**
   * Finds most optimal blast radius and self-destructs.
   */
  def selfDestruct(bot: MiniBot) {
    ExplosionAnalyzer.apply(bot, bot.energy)
    //bot.say("Goodbye![" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString + "]")
    bot.explode(ExplosionAnalyzer.bestRadius)
  }

  /**
   * Analyzes if it is valuable to explode now.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryValuableExplosion(bot: MiniBot): Boolean = {
    if (bot.slaves > (Const.LOWER_SPAWN_LIMIT * 0.4)) {
      var threshold = Const.VALUABLE_EXPLOSION_THRESHOLD
      if (bot.apocalypse < 1000) threshold = Const.VALUABLE_EXPLOSION_THRESHOLD * 0.75
      ExplosionAnalyzer.apply(bot, bot.energy)

      if (ExplosionAnalyzer.bestDamage > (bot.energy * threshold)) {
        if (Const.DEBUG) bot.say("VAL BOMB")
        bot.explode(ExplosionAnalyzer.bestRadius)
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
    if (bot.energy > 200) {
      ExplosionAnalyzer.apply(bot, 100)
      if (ExplosionAnalyzer.bestDamage > (100 * (Const.VALUABLE_EXPLOSION_THRESHOLD * 0.85))) {
        val relPos = bot.view.offsetToNearestEnemy()
        if (Const.DEBUG) bot.say("D BOMB")
        bot.spawn(relPos.signum.rotateClockwise45, "type" -> SlaveType.DROP_BOMB, "radius" -> ExplosionAnalyzer.bestRadius)
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
    bot.slaves < Const.UPPER_SPAWN_LIMIT &&
      bot.time > bot.inputAsIntOrElse("missileDelay", -1) &&
      bot.energy > 300 &&
      (bot.view.countType(CellType.ENEMY_SLAVE) > 0 || bot.view.countType(CellType.ENEMY_BEAST) > 2)
  }

  /**
   * Fire a missile.
   */
  def fireMissile(bot: Bot): Unit = {
    val relPos = bot.view.offsetToNearestEnemy()
    var fireRate = 3
    if (bot.energy > 30000 && bot.slaves < Const.LOWER_SPAWN_LIMIT) fireRate = 2

    val energy = (bot.energy / 40).min(300).max(100) + 5
    bot.spawn(relPos.signum, "type" -> SlaveType.MISSILE, "target" -> relPos.toDirection45, "energy" -> energy)
    bot.set("missileDelay" -> (bot.time + fireRate))
  }

  /**
   * Spawn a Hunter
   */
  def spawnHunter(bot: Bot, direction: XY): Unit = {
    bot.spawn(direction.signum, "target" -> direction.toDirection45, "type" -> SlaveType.HUNTER, "energy" -> 105)
    if (Const.DEBUG) bot.say("Hunter!")
  }

  /**
   * Spawn a Vampire
   */
  def spawnVampire(bot: Bot, direction: XY): Unit = {
    bot.spawn(direction.signum, "target" -> direction.toDirection45, "type" -> SlaveType.VAMPIRE, "energy" -> (bot.energy / 10).min(500).max(100))
    if (Const.DEBUG) bot.say("Vampire!")
  }

  /**
   * Check if in immediate danger.
   * If danger launch defence bot and return true,
   * else return false
   */
  def handleDanger(bot: Bot): Boolean = {
    val defenceTimeDelay = bot.inputAsIntOrElse("defenceDelay", 0)
    if (bot.time > defenceTimeDelay && bot.energy > 200) {
      val slave = bot.view.offsetToNearest('s')
      return slave match {
        case Some(pos: XY) =>
          if (pos.length <= 6) {
            if (Const.DEBUG) bot.say("Danger!")
            val energy = (((bot.energy / 50) / 100) * 100).min(200).max(100) + 3
            bot.spawn(pos.signum, "type" -> SlaveType.DEFENCE, "target" -> pos.toDirection45, "energy" -> energy)
            bot.set("defenceDelay" -> (bot.time + 2))
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
