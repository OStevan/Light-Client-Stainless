package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.BlockchainStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.SystemSteps.SystemStep
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Chain.Genesis
import stainless.annotation._
import stainless.lang._
import utils.ListSet

object BlockchainSystem {

  @ghost
  def initialSystem(
    faultChecker: FaultChecker,
    validatorSet: ValidatorSet,
    maxHeight: Height,
    maxPower: VotingPower,
    nextValidatorSet: ValidatorSet,
    initialCommit: Commit
  ): BlockchainState = {
    require(
      validatorSet.values.forall(_.votingPower.value == 1) &&
        maxPower.isPositive &&
        nextValidatorSet.keys.subsetOf(validatorSet.keys) &&
        faultChecker.isCorrect(nextValidatorSet, ListSet.empty) && initialCommit.forBlock.isEmpty)

    val noFaulty = ListSet.empty[Address]

    val genesisBlock =
      BlockHeader(Blockchain.constructHeader(Height(1), Timestamp(0, 0)), initialCommit, validatorSet, nextValidatorSet)
    val initialChain = Genesis(genesisBlock)
    val minTrustedTime = Timestamp(1, 0)

    val startingBlockchain: Blockchain =
      Blockchain(maxHeight, minTrustedTime, Duration(100, 0), initialChain, ListSet.empty, faultChecker)

    val allNodes = validatorSet.keys
    ListSet.lemmas.selfContained(allNodes)
    if (maxHeight.value == BigInt(1)) {
      Finished(allNodes, noFaulty, startingBlockchain)
    } else {
      Running(allNodes, noFaulty, maxPower, startingBlockchain)
    }
  }.ensuring(res => neverStuckFalse2(res))

  @ghost
  def systemInvariant(blockchainState: BlockchainState, message: SystemStep): Boolean = {
    neverStuckFalse2(blockchainState.step(message))
  }.holds

  /**
   * This invariant is basically NeverStuckFalse2 from the TLA spec. Ignoring the uninitialized state.
   *
   * @param blockchainState possible state
   * @return if the invariant holds
   */
  @ghost
  def neverStuckFalse2(blockchainState: BlockchainState): Boolean = {
    blockchainState.isInstanceOf[Faulty] ||
    blockchainState.isInstanceOf[Running] ||
    blockchainState.isInstanceOf[Finished]
  }

  /**
   * NeverFaulty from the TLA spec.
   *
   * @param blockchainState possible state
   * @return if the invariant holds
   */
  //  @ghost
  @inlineOnce
  def neverFaulty(blockchainState: BlockchainState): Boolean = blockchainState match {
    case _: Faulty => false
    case _ => true
  }

  /**
   * NeverStuckFalse1 check that the chain is always running
   *
   * @param blockchainState possible state
   * @return if the invariant holds
   */
  @ghost
  @inlineOnce
  def neverStuckFalse1(blockchainState: BlockchainState): Boolean = {
    blockchainState.isInstanceOf[Running]
  }

}
