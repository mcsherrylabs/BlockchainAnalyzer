package iohk.scorex.difficulty

import iohk.scorex.difficulty.Simulator.Difficulty

import scala.concurrent.duration.FiniteDuration

class BitcoinDifficultyControl extends DifficultyControl {
  override def diff(lastDiffs: Seq[(Difficulty, FiniteDuration)], desired: FiniteDuration): BigInt = {
    lastDiffs.head._1 * desired.toMillis / lastDiffs.head._2.toMillis
  }
}