package analyzers

import utils.{Const, MiniBot, XY}

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
    var bestDamage = 0
    var bestRadius = 0
    val visibleBots = bot.view.getRelPosForType('m') ++ bot.view.getRelPosForType('s') ++ bot.view.getRelPosForType('b')

    (Const.MinBlastRadius to Const.MaxBlastRadius).foreach(testRadius => {
      val damage = simulateExplosion(testRadius, energy, visibleBots, bot.time)
      if (damage >= bestDamage) {
        bestDamage = damage
        bestRadius = testRadius
      }
    })

    (bestRadius, bestDamage)
  }

  /**
   * Simulates an explosion on view and returns
   * estimated damage.
   */
  def simulateExplosion(blastRadiusIn: Int, energy: Int, bots: Array[(Char, XY)], time: Int): Int = {
    var totalDamage = 0

    bots.foreach {
      case (typeChar, pos) =>
        val distance = pos.distanceTo(XY.Zero)
        if (distance <= blastRadiusIn) {
          val rawDamage = calculateDamage(blastRadiusIn, energy, distance)
          totalDamage = totalDamage + typeChar match {
            case 'm' => Const.MaxMasterBot.min(rawDamage)
            case 's' => Const.MaxMiniBot.min(rawDamage)
            case 'b' => Const.MaxBadCreature.min(rawDamage)
            case _ => 0
          }
        }
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