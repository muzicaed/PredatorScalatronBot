package control

import utils.{Bot, MiniBot, XY}

/**
 * Shared control functions
 */
object SharedControl {

  /**
   * Moves bot in direction and stores as last direction.
   */
  def moveBotInDirection(bot: MiniBot, directionValue: Array[Double]) = {
    val lastDirection = bot.inputAsIntOrElse("lastDirection", 0)

    // determine movement direction
    directionValue(lastDirection) += 70 // try to break ties by favoring the last direction
    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val direction = XY.fromDirection45(bestDirection45)

    // If Mini-Bot and apocalypse closing in, head home!
    if (bot.generation > 0 && bot.apocalypse < 80) {
      val directionXY = bot.offsetToMaster.signum
      directionValue(directionXY.toDirection45) += 10000
    }

    bot.move(direction)
    bot.set("lastDirection" -> bestDirection45)
    direction
  }

  /**
   * Fire a missile.
   */
  def fireMissile(bot: Bot): Unit = {
    var fireRate = 3
    var power = 100
    if (bot.energy > 10000) {
      fireRate = 2
    } else if (bot.energy > 50000) {
      fireRate = 2
      power = 110
    } else if (bot.energy > 25000) {
      fireRate = 1
      power = 120
    }


    val relPos = bot.view.offsetToNearestEnemy()
    bot.spawn(relPos.signum, "type" -> "Missile", "target" -> relPos, "energy" -> power)
    bot.set("missileDelay" -> (bot.time + fireRate))
  }

  /**
   * Checks if now is a good time to fire
   * a missile.
   */
  def checkFireMissile(bot: Bot): Boolean = {
    (bot.view.countType('m') > 0 || bot.view.countType('s') > 0 || bot.view.countType('b') > 4) &&
      bot.time > bot.inputAsIntOrElse("missileDelay", -1) &&
      bot.energy > 300
  }

  /**
   * Checks if now is a good time to spawn a Vampire.
   */
  def checkVampireSpawn(bot: Bot): Boolean = {
    val vampireTimeCount = bot.inputAsIntOrElse("vampireTimeCount", 0)
    bot.set("vampireTimeCount" -> (vampireTimeCount + 1))
    //bot.view.countType('S') < 12 && (bot.energy > 8000 && vampireTimeCount > 15) || (bot.energy > 1500 && vampireTimeCount > 20) && bot.apocalypse > 50
    (bot.energy > 10000 && vampireTimeCount > 15) || (bot.energy > 1500 && vampireTimeCount > 20) && bot.view.countType('S') < 20 && bot.apocalypse > 50
  }

  /**
   * Spawn a Vampire
   */
  def spawnVampire(bot: Bot, moveDirection: XY): Unit = {
    bot.spawn(moveDirection.negate.signum, "type" -> "Vampire", "energy" -> (bot.energy / 10).min(100).max(4000))
    bot.set("vampireTimeCount" -> 0)
    bot.say("Rise from the dead!")
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
          if (pos.stepsTo(XY.Zero) <= 5) {
            bot.say("Danger!")
            bot.spawn(pos.signum, "type" -> "Defence", "target" -> pos, "energy" -> (bot.energy / 30).min(100))
            bot.set("defenceDelay" -> (bot.time + 3))
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