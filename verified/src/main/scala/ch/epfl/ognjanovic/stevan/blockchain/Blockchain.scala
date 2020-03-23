package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.types.Chain.{Chain, ChainLink}
import ch.epfl.ognjanovic.stevan.types.Nodes._
import ch.epfl.ognjanovic.stevan.types.{Height, _}
import stainless.lang._
import stainless.collection._
import stainless.math._
import stainless.annotation._

case class Blockchain(
                       maxHeight: Height,
                       minTrustedHeight: Height,
                       chain: Chain,
                       faulty: Set[Node]) {
  require(minTrustedHeight.value <= min(chain.height.value + 1, maxHeight.value)
    && chain.height.value <= maxHeight.value)
//    && minTrustedHeight.value <= maxHeight.value)

  def increaseMinTrustedHeight(step: BigInt): Blockchain = {
    require(step > BigInt(0))
    val newMinTrustedHeight =
      Height(
        min(min(maxHeight.value, chain.height.value + 1), (minTrustedHeight + step).value))
    Blockchain(maxHeight, newMinTrustedHeight, chain, faulty)
  }

  def faultAssumption(): Boolean = {
    chain.map(_.nextValidatorSet).slice(0, (chain.size - minTrustedHeight.value + 1))
      .forall(validatorSet => validatorSet.isCorrect(faulty))
  }

  def appendBlock(lastCommit: Set[Node], nextVS: Validators): Blockchain = {
    if (chain.height == maxHeight)
      this
    else {
      val header = BlockHeader(chain.height + 1, lastCommit, chain.head.nextValidatorSet, nextVS)
      val newChain = chain.appendBlock(header)
      Blockchain(maxHeight, minTrustedHeight, newChain, faulty)
    }
  }.ensuring(res => res.chain.height.value <= maxHeight.value &&
    res.minTrustedHeight == minTrustedHeight)

  def finished: Boolean = chain.height == maxHeight

  def size: BigInt = chain.size

  def height: Height = chain.height

  def setFaulty(newFaulty: Set[Node]): Blockchain = Blockchain(maxHeight, minTrustedHeight, chain, newFaulty)
}