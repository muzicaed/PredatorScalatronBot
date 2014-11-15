package analyzers

import utils.{CellType, XY, Const, MiniBot}

/**
 * Simulates explosions using different blast radius and returns
 * most effective blastRadius and estimated caused damage.
 */
object ExplosionAnalyzer {

  /**
   * Finds the optimal blast radius and how much damage
   * it would cause.
   * @return tuple (radius:Int, damage:Int)
   */
  def apply(bot: MiniBot, energy: Int): (Int, Int) = {
    if (bot.time % 2 == 0) {
      var bestDamage = 0
      var bestRadius = 0
      val visibleBots = bot.view.getRelPosForType(CellType.ENEMY_MASTER) ++
        bot.view.getRelPosForType(CellType.ENEMY_SLAVE) ++
        bot.view.getRelPosForType(CellType.ENEMY_BEAST)

      (Const.MinBlastRadius to Const.MaxBlastRadius).foreach(testRadius => {
        val damage = simulateExplosion(testRadius, energy, visibleBots, bot.time)
        if (damage >= bestDamage) {
          bestDamage = damage
          bestRadius = testRadius
        }
      })

      return (bestRadius, bestDamage)
    }
    (0, 0)
  }

  /**
   * Simulates an explosion on view and returns
   * estimated damage.
   */
  def simulateExplosion(blastRadiusIn: Int, energy: Int, bots: Array[(Char, XY)], time: Int): Int = {
    var totalDamage = 0
    var i = 0
    while (i < bots.length) {
      val tuple = bots(i)
      val distance = tuple._2.distanceTo(XY.Zero)
      if (distance <= blastRadiusIn) {
        val rawDamage = calculateDamage(blastRadiusIn, energy, distance)
        if (tuple._1 == 'm') totalDamage += Const.MaxMasterBot.min(rawDamage)
        else if (tuple._1 == 's') totalDamage += Const.MaxMiniBot.min(rawDamage)
        else if (tuple._1 == 'b') totalDamage += Const.MaxBadCreature.min(rawDamage)
      }
      i += 1
    }
    totalDamage
  }

  /**
   * Calculates the blast damage
   */
  def calculateDamage(blastRadiusIn: Int, energy: Int, distance: Double): Int = {
    val blastRadius =
      if (blastRadiusIn < Const.MinBlastRadius) Const.MinBlastRadius
      else if (blastRadiusIn > Const.MaxBlastRadius) Const.MaxBlastRadius
      else blastRadiusIn

    val blastArea = blastRadius * blastRadius * math.Pi
    val energyPerArea = energy / blastArea
    val damageAtCenter = Const.ExplosionDamageFactor * energyPerArea

    val distanceFactor = 1 - (distance / blastRadius)
    (damageAtCenter * distanceFactor).intValue
  }
}