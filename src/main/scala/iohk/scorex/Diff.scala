package iohk.scorex

import java.io.{BufferedWriter, File, FileWriter}

import iohk.scorex.Diff.Difficulty

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import BigInt._
import scala.util.Random

object Diff extends App {

  type HashRate = BigInt
  type Difficulty = BigInt
  type Duration = Long

  //  val diffControl = new BitcoinDifficultyControl
  val diffControl = new LinearDifficultyControl
  val desired = 10.minutes
  val R = 1000
  val Ra = 200
  val Blocks = 1000


//  def hashRate(i: Int, prev: HashRate): BigInt = BigInt(1000)

//      def hashRate(i: Int, prev: HashRate): BigInt = prev * 110 / 100
//      def hashRate(i: Int, prev: HashRate): BigInt = if (i % 2 == 1) R else R + Ra
    def hashRate(i: Int, prev: HashRate): BigInt = prev * (900 + Random.nextInt(204)) / 1000

  def simulate(i: Int, acc: Seq[(Int, Difficulty, HashRate, FiniteDuration)], maxI: Int): Seq[(Int, Difficulty, HashRate, FiniteDuration)] = if (i < maxI) {
    val R = hashRate(i, acc.head._3)
    val newDiff = diffControl.diff(acc.take(5).map(a => a._2 -> a._4), desired)
    val realDuration = (desired.toMillis * newDiff / R).toInt.millis
    simulate(i + 1, (i, newDiff, R, realDuration) +: acc, maxI)
  } else acc

  val res = simulate(1, Seq((0, BigInt(1000), BigInt(1000), desired)), Blocks).reverse
  val file = new File("data/Linear/random.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  res.foreach(r => bw.write(r._1 + " " + r._2 + " " + r._3 + " " + r._4.toMillis + "\n"))
  bw.write((res.map(_._4.toMillis).sum / res.length) + "\n")
  bw.close()


}

sealed trait DifficultyControl {
  def diff(lastDiffs: Seq[(Difficulty, FiniteDuration)], desired: FiniteDuration): BigInt
}

class BitcoinDifficultyControl extends DifficultyControl {
  override def diff(lastDiffs: Seq[(Difficulty, FiniteDuration)], desired: FiniteDuration): BigInt = {
    lastDiffs.head._1 * desired.toMillis / lastDiffs.head._2.toMillis
  }
}

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