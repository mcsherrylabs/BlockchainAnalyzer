package iohk.scorex.difficulty

import iohk.scorex.difficulty.Simulator.Difficulty

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl extends DifficultyControl {

  override def diff(lastDiffs: Seq[(Difficulty, FiniteDuration)], desired: FiniteDuration): BigInt = {
    if (lastDiffs.size > 2) {
      val realDiffs: Seq[BigInt] = lastDiffs.reverse.map(l => l._1 * desired.toMillis / l._2.toMillis).toSeq
      val data: Seq[(Int, Difficulty)] = (0 until realDiffs.size).map(i => i -> realDiffs(i))
      interpolate(data)(data.map(_._1).max + 1)
    } else lastDiffs.head._1
  }

  //y = a + bx
  def interpolate(data: Seq[(Int, BigInt)]): (Int) => BigInt = {
    val size = data.size
    val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
    val x: Iterable[Int] = data.map(d => d._1)
    val x2: Iterable[Int] = data.map(d => d._1 * d._1)
    val y: Iterable[BigInt] = data.map(d => d._2)
    val xyMean = xy.sum / size
    val x2Mean = x2.sum / size
    val yMean = y.sum / y.size

    val b = (xyMean * size - x.sum * yMean) * size  / (x2.sum * size - (x.sum * x.sum))
    val a = yMean - b * x.sum / size
    (point: Int) => {
      a + b * point
    }
  }

}