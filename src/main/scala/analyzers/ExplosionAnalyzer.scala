package analyzers

import utils.{MiniBot, XY}

/**
 * Simulates explosions using different blast radius and returns
 * most effective blastRadius and estimated caused damage.
 */
object ExplosionAnalyzer {

  val MinBlastRadius = 2
  val MaxBlastRadius = 10
  val ExplosionDamageFactor = 200
  val MaxBadCreature = 200
  val MaxMiniBot = 100

  /**
   * Finds the optimal blast radius and how much damage
   * it would cause.
   * @return tuple (radius:Int, damage:Int)
   */
  def apply(bot: MiniBot, energy: Int): (Int, Int) = {
    var bestDamage = 0
    var bestRadius = 0
    val visibleBots = bot.view.getRelPosForType('m') ::: bot.view.getRelPosForType('s') ::: bot.view.getRelPosForType('b')

    (MinBlastRadius to MaxBlastRadius).foreach(testRadius => {
      val damage = simulateExplosion(testRadius, energy, visibleBots, bot.time)
      if (damage > bestDamage) {
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
  def simulateExplosion(blastRadiusIn: Int, energy: Int, bots: List[(Char, XY)], time: Int): Int = {
    var totalDamage = 0

    bots.foreach {
      case (typeChar, pos) => {
        val distance = pos.distanceTo(XY.Zero)
        if (distance <= blastRadiusIn) {
          val rawDamage = calculateDamage(blastRadiusIn, energy, distance)
          totalDamage = totalDamage + typeChar match {
            case 'm' => ((time * 2).min(300)).min(rawDamage)
            case 's' => MaxMiniBot.min(rawDamage)
            case 'b' => MaxBadCreature.min(rawDamage)
            case _ => 0
          }
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
      if (blastRadiusIn < MinBlastRadius) MinBlastRadius
      else if (blastRadiusIn > MaxBlastRadius) MaxBlastRadius
      else blastRadiusIn

    val blastArea = blastRadius * blastRadius * math.Pi
    val energyPerArea = energy / blastArea
    val damageAtCenter = ExplosionDamageFactor * energyPerArea

    val distanceFactor = 1 - (distance / blastRadius)
    (damageAtCenter * distanceFactor).intValue
  }
}