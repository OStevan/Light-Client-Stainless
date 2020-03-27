package ch.epfl.ognjanovic.stevan.blockchain;

import ch.epfl.ognjanovic.stevan.blockchain.BlockchainStates._
import ch.epfl.ognjanovic.stevan.blockchain.SystemSteps.SystemStep
import ch.epfl.ognjanovic.stevan.types.Chain.Genesis
import ch.epfl.ognjanovic.stevan.types.{BlockHeader, Height, Validators, VotingPower}
import stainless.lang._
import stainless.annotation._
import stainless.collection._


object BlockchainSystem {

  @ghost
  def initialSystem(
                     validatorSet: Validators,
                     maxHeight: Height,
                     maxPower: VotingPower,
                     nextValidatorSet: Validators): BlockchainState = {
    require(
      validatorSet.keys.nonEmpty &&
        validatorSet.values.forall(value => value.power == 1) &&
        maxPower.isPositive &&
        nextValidatorSet.keys.nonEmpty &&
        (nextValidatorSet.keys subsetOf validatorSet.keys))

    val genesisBlock = BlockHeader(Height(1), Set.empty, validatorSet, nextValidatorSet)
    val initialChain = Genesis(genesisBlock)
    val minTrustedHeight = Height(1)
    assert(initialChain.height <= maxHeight) // without this assertion, infinite verification
    val startingBlockchain = Blockchain(maxHeight, minTrustedHeight, initialChain, Set.empty)
    if (maxHeight.value == BigInt(1))
      Finished(startingBlockchain)
    else
      Running(validatorSet.keys, Set.empty, maxPower, startingBlockchain)
  }.ensuring(res => neverStuckFalse2(res))

//  def notCaughtTransition(): BlockchainState = {
//    val node = SimpleNode(1)
//    val allNodes: Set[Node] = Set(node, SimpleNode(2))
//    val nodePower: ListMap[Node, VotingPower] = ListMap(List((node, VotingPower(1)), (SimpleNode(2), VotingPower(1))))
//    val validators = Validators(VotingPower(2), nodePower)
//    val firstHeader = BlockHeader(Height(1), Set.empty, validators, validators)
//    val secondHeader = BlockHeader(Height(2), allNodes, validators, validators)
//    val blockchain = Blockchain(Height(3), Height(1), ChainLink(secondHeader, Genesis(firstHeader)), Set.empty)
//    val blockchainState = Running(allNodes, Set.empty, VotingPower(1), blockchain)
//
//    val faulty = Fault(node)
//
//    val result = blockchainState.step(faulty)
//    assert(neverFaulty(result))
//    result
//  }

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
  @inlineOnce
  def neverStuckFalse2(blockchainState: BlockchainState): Boolean = {
    blockchainState.isInstanceOf[Faulty] ||
      (blockchainState.isInstanceOf[Running] ||
        blockchainState.isInstanceOf[Finished])
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
