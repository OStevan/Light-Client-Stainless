package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.types.Chain._
import ch.epfl.ognjanovic.stevan.types.Height._
import ch.epfl.ognjanovic.stevan.types.Nodes._
import ch.epfl.ognjanovic.stevan.types.SignedHeader.{DefaultSignedHeader, SignedHeader}
import ch.epfl.ognjanovic.stevan.types.{Chain => _, _}
import stainless.annotation.pure
import stainless.lang._

case class Blockchain(maxHeight: Height, minTrustedHeight: Height, chain: Chain, faulty: Set[Node]) {
  require(
    minTrustedHeight <= min(chain.height + 1, maxHeight) &&
      chain.height <= maxHeight)

  @inline
  def increaseMinTrustedHeight(step: BigInt): Blockchain = {
    require(step > BigInt(0))
    val newMinTrustedHeight = min(min(maxHeight, chain.height + 1), minTrustedHeight + step)
    Blockchain(maxHeight, newMinTrustedHeight, chain, faulty)
  }

  @inline
  def faultAssumption(): Boolean = {
    chain.map(id => id)
      .filter(header => minTrustedHeight <= header.height)
      .forall(header => header.nextValidatorSet.isCorrect(faulty))
  }

  @inline
  @pure
  def appendBlock(lastCommit: Set[Node], nextVS: Validators): Blockchain = {
    require(nextVS.keys.nonEmpty && lastCommit.nonEmpty && !finished)
    val header = BlockHeader(chain.height + 1, lastCommit, chain.head.nextValidatorSet, nextVS)
    val newChain = chain.appendBlock(header)
    Blockchain(maxHeight, minTrustedHeight, newChain, faulty)
  }.ensuring(res =>
    res.chain.height <= maxHeight &&
      res.minTrustedHeight == minTrustedHeight &&
      res.chain.head.validatorSet == chain.head.nextValidatorSet)

  @inline
  def finished: Boolean = {
    chain.height == maxHeight
  }.ensuring(res => !res ==> chain.height < maxHeight)

  def size: BigInt = chain.size

  def height: Height = chain.height

  @inline
  def setFaulty(newFaulty: Set[Node]): Blockchain = Blockchain(maxHeight, minTrustedHeight, chain, newFaulty)

  def getHeader(height: Height): BlockHeader = {
    require(height <= chain.height)
    getHeaderInternal(height, chain)
  }

  def getSignedHeader(height: Height): SignedHeader = {
    require(height < chain.height)
    val headerCommit = getHeader(height + 1).lastCommit
    val blockHeader = getHeader(height)
    DefaultSignedHeader(blockHeader, headerCommit)
  }

  @scala.annotation.tailrec
  private def getHeaderInternal(height: Height, chain: Chain): BlockHeader = {
    require(height <= chain.height)
    chain match {
      case Genesis(blockHeader) =>
        assert(height == chain.height)
        blockHeader
      case ChainLink(blockHeader, tail) =>
        if (height == chain.height)
          blockHeader
        else
          getHeaderInternal(height, tail)
    }
  }
}
