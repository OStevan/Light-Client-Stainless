package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Chain._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Chain => _, _}
import stainless.annotation.{extern, induct, opaque, pure}
import stainless.lang._

case class Blockchain(
  maxHeight: Height,
  minTrustedHeight: Height,
  chain: Chain,
  faulty: Set[Address],
  faultChecker: FaultChecker) {
  require(
    minTrustedHeight <= min(chain.height + 1, maxHeight) &&
      chain.height <= maxHeight)

  @pure
  def increaseMinTrustedHeight(step: BigInt): Blockchain = {
    require(step > BigInt(0))
    val newMinTrustedHeight = min(min(maxHeight, chain.height + 1), minTrustedHeight + step)
    Blockchain.timeProgressDoesNotDeteriorateTheState(
      faultChecker,
      newMinTrustedHeight,
      minTrustedHeight,
      chain,
      faulty)
    Blockchain(maxHeight, newMinTrustedHeight, chain, faulty, faultChecker)
  }.ensuring(res => faultAssumption() ==> res.faultAssumption())

  @pure
  def faultAssumption(): Boolean = {
    chain.forAll(header =>
      minTrustedHeight > header.header.height || faultChecker.isCorrect(header.nextValidatorSet, faulty))
  }

  @pure
  def appendBlock(lastCommit: Commit, nextVS: ValidatorSet): Blockchain = {
    require(
        !finished &&
        faultChecker.isCorrect(nextVS, faulty) &&
        faultAssumption())
    val header = BlockHeader(
      Blockchain.constructHeader(chain.height + 1),
      lastCommit,
      chain.head.nextValidatorSet, nextVS)
    val newChain = chain.appendBlock(header)
    Blockchain(maxHeight, minTrustedHeight, newChain, faulty, faultChecker)
  }.ensuring(res =>
    res.faultAssumption() &&
      res.height == height + 1 &&
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
  def setFaulty(newFaulty: Set[Address]): Blockchain = {
    require(faulty subsetOf newFaulty)
    Blockchain.faultyChainDoesNotRecoverWithNewFault(faultChecker, minTrustedHeight, chain, faulty, newFaulty)
    Blockchain(maxHeight, minTrustedHeight, chain, newFaulty, faultChecker)
  }.ensuring(res => !faultAssumption() ==> !res.faultAssumption())

  def getHeader(height: Height): BlockHeader = {
    require(height <= chain.height)
    getHeaderInternal(height, chain)
  }.ensuring(res => res.header.height == height)

  def getLightBlock(height: Height): LightBlock = {
    require(height < chain.height)
    val headerCommit = getHeader(height + 1).lastCommit
    val blockHeader = getHeader(height)
    LightBlock(
      blockHeader.header,
      headerCommit,
      blockHeader.validatorSet,
      blockHeader.nextValidatorSet,
      Blockchain.peer())
  }

  private def getHeaderInternal(height: Height, chain: Chain): BlockHeader = {
    require(height <= chain.height)
    chain match {
      case Genesis(blockHeader) => blockHeader
      case ChainLink(blockHeader, tail) =>
        if (height == chain.height)
          blockHeader
        else
          getHeaderInternal(height, tail)
    }
  }.ensuring(res => res.header.height == height)
}

object Blockchain {

  import StaticChecks.Ensuring

  // strictly for modeling purposes, anything which runs this will fail
  @extern
  @pure
  def constructHeader(height: Height): Header = {
    ??? : Header
  }.ensuring(res => res.height == height)

  // models an ID of a peer the light client is connected to, strictly for modeling purposes
  @extern
  @pure
  def peer(): PeerId = {
    ??? : PeerId
  }

  @opaque
  def faultyChainDoesNotRecoverWithNewFault(
    faultChecker: FaultChecker,
    minTrustedHeight: Height,
    @induct chain: Chain,
    faulty: Set[Address],
    newFaulty: Set[Address]): Unit = {
    require(faulty subsetOf newFaulty)
  }.ensuring { _ =>
    FaultChecker.moreFaultyDoesNotHelp(faultChecker, faulty, newFaulty)
    !chain.forAll(
      header => minTrustedHeight > header.header.height || faultChecker.isCorrect(header.nextValidatorSet, faulty)) ==>
      !chain.forAll(
        header => minTrustedHeight > header.header.height || faultChecker.isCorrect(header.nextValidatorSet, newFaulty))
  }

  @opaque
  def timeProgressDoesNotDeteriorateTheState(
    faultChecker: FaultChecker,
    newMinTrustedState: Height,
    minTrustedHeight: Height,
    @induct chain: Chain,
    faulty: Set[Address]): Unit = {
    require(newMinTrustedState >= minTrustedHeight)
  }.ensuring { _ =>
    chain.forAll(header => minTrustedHeight > header.header.height || faultChecker.isCorrect(header.nextValidatorSet, faulty)) ==>
      chain.forAll(header => newMinTrustedState > header.header.height || faultChecker.isCorrect(header.nextValidatorSet, faulty))
  }
}
