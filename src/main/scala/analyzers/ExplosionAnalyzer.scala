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

      (Const.MIN_BLAST_RADIUS to Const.MAX_BLAST_RADIUS).foreach(testRadius => {
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
      val distance = tuple._2.length
      if (distance <= blastRadiusIn) {
        val rawDamage = calculateDamage(blastRadiusIn, energy, distance)
        if (tuple._1 == 'm') totalDamage += Const.MAX_DAMAGE_MASTER.min(rawDamage)
        else if (tuple._1 == 's') totalDamage += Const.MAX_DAMAGE_SLAVE.min(rawDamage)
        else if (tuple._1 == 'b') totalDamage += Const.MAX_DAMAGE_BEAST.min(rawDamage)
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
      if (blastRadiusIn < Const.MIN_BLAST_RADIUS) Const.MIN_BLAST_RADIUS
      else if (blastRadiusIn > Const.MAX_BLAST_RADIUS) Const.MAX_BLAST_RADIUS
      else blastRadiusIn

    val blastArea = blastRadius * blastRadius * math.Pi
    val energyPerArea = energy / blastArea
    val damageAtCenter = Const.EXPLOSION_DAMAGE_FACTOR * energyPerArea

    val distanceFactor = 1 - (distance / blastRadius)
    (damageAtCenter * distanceFactor).intValue
  }
}