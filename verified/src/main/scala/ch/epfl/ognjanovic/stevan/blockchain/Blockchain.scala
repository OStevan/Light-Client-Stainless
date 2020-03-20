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
  require(
    chain.nonEmpty && // there at least has to one block(genesis block)
    chain.size == height.value + BigInt(1) && // height of the last block should be the same as the chain size
    chain.head.height == height &&
    chain.map(_.height.value).forall(value => chain.size > value)
//      chain.size == chain.map(_.height).content.toList.size //&& // each block should have a unique height
//      ListOps.isSorted(chain.map(_.height.value).reverse) // force that heights are sorted
   )

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
//    require(ListOps.isSorted(chain.map(_.height.value).reverse))
    val header = BlockHeader(Height(chain.size), lastCommit, validatorSet, nextVS)
    val newChain = Cons(header, chain)
    Blockchain(Height(chain.size), minTrustedHeight, newChain, faulty)
  }
//  ensuring(res => ListOps.isSorted(res.chain.map(_.height.value).reverse))

  def oneMore(maxHeight: Height): Boolean = height.value + 1 == maxHeight.value

  def size(): BigInt = chain.size

  def setFaulty(newFaulty: Set[Node]): Blockchain = Blockchain(height, minTrustedHeight, chain, newFaulty)
}
