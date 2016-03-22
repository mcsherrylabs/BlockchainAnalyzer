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

  val diffControl = new BitcoinDifficultyControl
  val desired = 10.minutes
  val R = 1000
  val Ra = 200


  //  def hashRate(i: Int): BigInt = BigInt(1000)
  //  def hashRate(i: Int, prev: HashRate): BigInt = prev * 110 / 100
//  def hashRate(i: Int, prev: HashRate): BigInt = if (i % 2 == 1) R else R + Ra
  def hashRate(i: Int, prev: HashRate): BigInt = prev * (900 + Random.nextInt(204)) / 1000

  def simulate(i: Int, acc: Seq[(Int, Difficulty, HashRate, Long)], maxI: Int): Seq[(Int, Difficulty, HashRate, Long)] = if (i < maxI) {
    val R = hashRate(i, acc.head._3)
    println(R)
    val realDuration = (desired.toMillis * acc.head._2 / R).toInt.millis
    val newDiff = diffControl.diff(acc.take(5).map(_._2), realDuration, desired)
    simulate(i + 1, (i, newDiff, R, realDuration.toMillis) +: acc, maxI)
  } else acc

  val res = simulate(1, Seq((0, BigInt(1000), BigInt(1000), 0L)), 1000).reverse
  val file = new File("D-B-rand.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  res.foreach(r => bw.write(r._1 + " " + r._2 + " " + r._3 + " " + r._4 + "\n"))
  bw.write((res.map(_._4).sum / res.length) + "\n")
  bw.close()


}

sealed trait DifficultyControl {
  def diff(lastDiffs: Seq[Difficulty], real: FiniteDuration, desired: FiniteDuration): BigInt
}

class BitcoinDifficultyControl extends DifficultyControl {
  override def diff(lastDiffs: Seq[BigInt], real: FiniteDuration, desired: FiniteDuration): BigInt = {
    lastDiffs.head * desired.toMillis / real.toMillis
  }
}

class LinearDifficultyControl extends DifficultyControl {
  override def diff(lastDiffs: Seq[Difficulty], real: FiniteDuration, desired: FiniteDuration): BigInt = {
    if(lastDiffs.size > 2) {
      val data = (1 to lastDiffs.size).map(i => i->lastDiffs(i)).toMap
      interpolate(data)(data.map(_._1).max + 1)
    } else lastDiffs.head
  }

  //y = a + bx
  private def interpolate(data:Map[Int, BigInt]): (Int) => BigInt = {
    val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
    val x: Iterable[Int] = data.map(d => d._1 )
    val x2: Iterable[Int] = data.map(d => d._1* d._1)
    val y: Iterable[BigInt] = data.map(d => d._2)
    val xyMean = xy.sum / xy.size
    val xMean = x.sum / x.size
    val x2Mean = x2.sum / x2.size
    val yMean = y.sum / y.size

    val b = (xyMean-xMean*yMean) / (x2Mean - xMean*xMean)
    val a = yMean - b*xMean
    (point: Int) => {
      a + b * point
    }
  }

}