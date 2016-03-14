package iohk.scorex

import java.io.{BufferedWriter, File, FileWriter}
import java.math.BigInteger
import java.nio.ByteBuffer
import java.security.SecureRandom
import java.util

import org.apache.commons.math3.stat.inference.ChiSquareTest
import org.bitcoinj.core.AbstractBlockChain.NewBlockType
import org.bitcoinj.core._
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.params.MainNetParams
import org.bitcoinj.store.SPVBlockStore
import org.mapdb.DBMaker

import scala.collection.JavaConversions._
import scala.util.Random


object BitcoinAnalyzer {
  val Folder = "/opt/scorex/BitcoinAnalyzer/"
  new File(Folder).mkdirs()
  val RetargetTimestamp = 2016

  val netParams = MainNetParams.get()
  lazy val store = new SPVBlockStore(netParams, new java.io.File(Folder + "blochchain"))
  lazy val ls: List[BlockChainListener] = List(listener)
  lazy val chain = new BlockChain(netParams, ls, store)
  lazy val MaxHeight = store.getChainHead.getHeight


  val db = DBMaker.fileDB(new File(Folder + "db"))
    .closeOnJvmShutdown()
    .checksumEnable()
    .make()

  val difficulty = db.treeMap[Int, Long]("Difficulty")
  val timestamp = db.treeMap[Int, Long]("timestamp")


  val listener = new BlockChainListener {
    override def reorganize(splitPoint: StoredBlock,
                            oldBlocks: util.List[StoredBlock],
                            newBlocks: util.List[StoredBlock]): Unit = {
      Unit
    }


    override def notifyNewBestBlock(block: StoredBlock): Unit = {
      difficulty.put(block.getHeight, block.getHeader.getDifficultyTarget)
      timestamp.put(block.getHeight, block.getHeader.getTimeSeconds)
      if (Random.nextInt(1000) == 1) {
        println("db.commit()")
        db.commit()
      }

      println("height: " + block.getHeight + ", difficulty: " + block.getHeader.getDifficultyTarget + ", timestamp:" +
        block.getHeader.getTimeSeconds)
    }

    override def notifyTransactionIsInBlock(txHash: Sha256Hash,
                                            block: StoredBlock,
                                            blockType: NewBlockType,
                                            relativityOffset: Int): Boolean = {
      println("random for: " + block.getHeight + " value: " + block.getHeader.getDifficultyTarget)
      false
    }

    override def receiveFromBlock(tx: Transaction,
                                  block: StoredBlock,
                                  blockType: NewBlockType,
                                  relativityOffset: Int): Unit = {

    }

    override def isTransactionRelevant(tx: Transaction): Boolean = {
      false
    }
  }

  def chainDownload(): Unit = {


    val peerGroup = new PeerGroup(netParams, chain)
    peerGroup.setUserAgent("SmartContract.com", "0.1")
    peerGroup.addPeerDiscovery(new DnsDiscovery(netParams))
    peerGroup.startAsync()
    peerGroup.downloadBlockChain()
  }

  def analyzeSeq(randoms: Seq[Long]) = {
    def secureRandoms(size: Int): Array[Long] = {
      new SecureRandom().longs(size, 0, Long.MaxValue).toArray
    }

    def scalaRandoms(size: Int): Array[Long] = {
      (1 to size).map(_ => Random.nextInt(Int.MaxValue).toLong).toArray
    }

    def longToBytes(l: Long) = ByteBuffer.allocate(8).putLong(l).array()


    println("Nonce collection size: " + randoms.size)
    println("ints only: " + randoms.forall(_ <= Int.MaxValue * 2 + 1))


    val evs = randoms.filter(_ % 2 == 0)
    println("evens: " + evs.size)

    val odds = randoms.filter(_ % 2 != 0)
    println("odds: " + odds.size)

    val max = randoms.max
    val median = max / 2
    val less = randoms.count(_ < median)
    val notless = randoms.count(_ >= median)
    println("less: " + less)
    println("notless: " + notless)

    val r10 = randoms.groupBy(_ % 10).mapValues(_.size.toLong)
    println(r10)

    val srs = secureRandoms(randoms.size)
    println("srs size: " + srs.length)
    println("evens-srs: " + srs.count(_ % 2 == 0))
    println("odds-srs: " + srs.count(_ % 2 != 0))
    val n10 = srs.groupBy(_ % 10).mapValues(_.length.toLong)
    println(n10)

    val scrs = scalaRandoms(randoms.size)
    println("srs size: " + scrs.length)
    println("evens-srs: " + scrs.count(_ % 2 == 0))
    println("odds-srs: " + scrs.count(_ % 2 != 0))
    val sc10 = scrs.groupBy(_ % 10).mapValues(_.length.toLong)
    println(sc10)

    val a = new ChiSquareTest().chiSquareDataSetsComparison(r10.values.toArray, n10.values.toArray)
    println(a)

    val c = new ChiSquareTest().chiSquareDataSetsComparison(r10.values.toArray, sc10.values.toArray)
    println(c)

    val b = new ChiSquareTest().chiSquareDataSetsComparison(sc10.values.toArray, n10.values.toArray)
    println(b)

  }

  def analyze(): Unit = {
    val randoms = db.treeMap[Int, Long]("randoms").filter(_._1 > 360000)

    randoms.values.toSeq.grouped(3000).foreach(analyzeSeq)

    println("Analysis is done")
  }

  def analyzeDiff(): Unit = {
    val heights = (1 to MaxHeight by RetargetTimestamp)
    val file = new File("difficulties.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    heights.foreach(i => bw.write(i.toString + " " + getDifficultyTargetAsInteger(difficulty.get(i)).toString + "\n"))
    bw.close()
  }

  def analyzeTimestamps(): Unit = {
    val heights = (1 to (MaxHeight - RetargetTimestamp) by RetargetTimestamp)
    var diffs: IndexedSeq[Long] = (0 until  RetargetTimestamp).map(i => 0L)
    println(diffs)
    heights.foreach { h =>
      println(h)
      (0 until RetargetTimestamp).foreach { i =>
        val current = diffs(i)
        val diff: Long = (timestamp.get(h + i + 1) - timestamp.get(h + i))
        if(diff < 0) println(s"WARN: ${h + i}=>$diff")
        diffs = diffs.updated(i, diff + current)
      }
    }
    val meanIntervals:Seq[Long] = diffs.map(_ / heights.size)
    val file = new File("intervals.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    meanIntervals.foreach(i => bw.write(i + "\n"))
    bw.close()
    val mean:Long = meanIntervals.sum / meanIntervals.size
    println(s"Mean = $mean")
  }
  //Fri, 26 Jun 2009 21:49:21 GMT
  // Fri, 26 Jun 2009 22:15:22 GMT 25*60 =  1500

  def recoverChain(): Unit = {
    val last = store.getChainHead.getHeader
    def loop(hash: Sha256Hash): Unit = {
      val block = store.get(hash)
      listener.notifyNewBestBlock(block)
      loop(block.getHeader.getPrevBlockHash)
    }
    loop(last.getHash)

  }

  def getDifficultyTargetAsInteger(difficultyTarget: Long): BigInteger = {
    return Utils.decodeCompactBits(difficultyTarget)
  }

  def main(args: Array[String]): Unit = {
    //    chainDownload()
    //    db.commit()
    //    analyzeDiff()
    analyzeTimestamps()

//    println(store.getChainHead.getHeader.getDifficultyTargetAsInteger)
  }
}