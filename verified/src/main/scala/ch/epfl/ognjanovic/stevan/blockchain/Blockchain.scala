package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.types.Nodes._
import ch.epfl.ognjanovic.stevan.types.{Height, _}
import stainless.lang._
import stainless.collection._
import stainless.math._

case class Blockchain(
                       height: Height,
                       minTrustedHeight: Height,
                       chain: List[BlockHeader],
                       faulty: Set[Node]) {
  require(chain.nonEmpty && chain.size == height.value + BigInt(1))

  def increaseMinTrustedHeight(step: BigInt, maxHeight: Height): Blockchain = {
    require(step > BigInt(0))
    val newMinTrustedHeight =
      Height(
        min(min(maxHeight.value, height.value + 1), minTrustedHeight.value + step))
    Blockchain(height, newMinTrustedHeight, chain, faulty)
  }

  def chainFault(): Boolean = {
    chain.slice(min(minTrustedHeight.value, chain.length), chain.length)
      .forall(header => header.nextValidatorSet.isCorrect(faulty))
  }

  def appendBlock(lastCommit: Set[Node], validatorSet: Validators, nextVS: Validators): Blockchain = {
    val header = BlockHeader(Height(chain.size), lastCommit, validatorSet, nextVS)
    Blockchain(Height(height.value + BigInt(1)), minTrustedHeight, header :: chain, faulty)
  }

  def oneMore(maxHeight: Height): Boolean = height.value + 1 == maxHeight.value

  def size(): BigInt = chain.size

  def setFaulty(newFaulty: Set[Node]): Blockchain =
    Blockchain(height, minTrustedHeight, chain, newFaulty)
}
