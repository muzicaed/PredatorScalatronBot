package analyzers

import utils.{CellType, Const, MiniBot, XY}

/**
 * Simulates explosions using different blast radius and returns
 * most effective blastRadius and estimated caused damage.
 */
object ExplosionAnalyzer {

  var bestDamage = 0
  var bestRadius = 0

  /**
   * Finds the optimal blast radius and how much damage
   * it would cause.
   * @return tuple (radius:Int, damage:Int)
   */
  def apply(bot: MiniBot, energy: Int) = {
    bestDamage = 0
    bestRadius = 0
    val visibleBots = bot.view.getRelPosForType(CellType.ENEMY_MASTER) ++
      bot.view.getRelPosForType(CellType.ENEMY_SLAVE) ++
      bot.view.getRelPosForType(CellType.ENEMY_BEAST)

    var testRadius = Const.MIN_BLAST_RADIUS
    while (testRadius <= Const.MAX_BLAST_RADIUS) {
      val damage = simulateExplosion(testRadius, energy, visibleBots, bot.time)
      if (damage >= bestDamage) {
        bestDamage = damage
        bestRadius = testRadius
      }
      testRadius += 1
    }
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