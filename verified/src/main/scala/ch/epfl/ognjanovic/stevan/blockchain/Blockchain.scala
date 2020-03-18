package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.library.Nodes._
import ch.epfl.ognjanovic.stevan.library.{Height, _}

case class Blockchain(
                       tooManyFaults: Boolean,
                       height: Height,
                       minTrustedHeight: Height,
                       chain: List[BlockHeader],
                       faulty: Set[Node]) {
  require(chain.nonEmpty && chain.size == height.value + BigInt(1))

  def chainFault(): Boolean = {
    chain.slice(min(minTrustedHeight.value, chain.length), chain.length)
      .forall(header => header.nextValidatorSet.isCorrect(faulty))
  }

  def appendBlock(lastCommit: Set[Node], validatorSet: Validators, nextVS: Validators): Blockchain = {
    val header = BlockHeader(Height(chain.size), lastCommit, validatorSet, nextVS)
    Blockchain(tooManyFaults, Height(height.value + BigInt(1)), minTrustedHeight, header :: chain, faulty)
  }

  def oneMore(maxHeight: Height): Boolean = height.value + 1 == maxHeight.value

  def size(): BigInt = chain.size

  def setFaulty(newFaulty: Set[Node]): Blockchain =
    Blockchain(tooManyFaults, height, minTrustedHeight, chain, newFaulty)
}
