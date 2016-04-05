package iohk.scorex.bitcoin

import java.util

import org.bitcoinj.core.AbstractBlockChain.NewBlockType
import org.bitcoinj.core.{Transaction, Sha256Hash, StoredBlock, BlockChainListener}

import scala.util.Random

abstract class Listener extends BlockChainListener {
  override def reorganize(splitPoint: StoredBlock,
                          oldBlocks: util.List[StoredBlock],
                          newBlocks: util.List[StoredBlock]): Unit = {
    Unit
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
