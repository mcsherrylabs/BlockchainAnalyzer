package iohk.scorex.difficulty

import iohk.scorex.difficulty.Simulator.Difficulty

import scala.concurrent.duration.FiniteDuration

trait DifficultyControl {
  def diff(lastDiffs: Seq[(Difficulty, FiniteDuration)], desired: FiniteDuration): BigInt
}