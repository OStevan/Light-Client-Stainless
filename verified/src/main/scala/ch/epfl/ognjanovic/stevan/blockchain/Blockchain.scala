package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.library.types.Height
import ch.epfl.ognjanovic.stevan.library.Nodes._
import ch.epfl.ognjanovic.stevan.library._
import stainless.lang._
import stainless.collection._
import stainless.math._

case class Blockchain(tooManyFaults: Boolean, height: Height, minTrustedHeight: Height, chain: List[BlockHeader], faulty: Set[Node]) {
    require(chain.nonEmpty && chain.size == height.value + BigInt(1))

    def chainFault(): Boolean = {
        chain.slice(min(minTrustedHeight.value, chain.length), chain.length).forall(header => header.nextValidatorSet.isCorrect(faulty))
    }

    def appendBlock(header: BlockHeader): Blockchain = {
        Blockchain(tooManyFaults, Height(height.value + BigInt(1)), minTrustedHeight, header :: chain, faulty)
    }

    def size(): BigInt = chain.size
}
