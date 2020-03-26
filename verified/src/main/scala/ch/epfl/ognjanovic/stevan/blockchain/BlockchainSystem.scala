package ch.epfl.ognjanovic.stevan.blockchain;

import ch.epfl.ognjanovic.stevan.blockchain.BlockchainStates._
import ch.epfl.ognjanovic.stevan.blockchain.Messages.{Fault, SystemStep}
import ch.epfl.ognjanovic.stevan.types.Chain.{ChainLink, Genesis}
import ch.epfl.ognjanovic.stevan.types.Nodes.{Node, SimpleNode}
import ch.epfl.ognjanovic.stevan.types.{BlockHeader, Height, Validators, VotingPower}
import stainless.lang._
import stainless.annotation._
import stainless.collection._

@ghost
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
    assert(initialChain.height.value <= maxHeight.value) // without this assertion, infinite verification
    val startingBlockchain = Blockchain(maxHeight, minTrustedHeight, initialChain, Set.empty)
    if (maxHeight.value == BigInt(1))
      Finished(startingBlockchain)
    else
      Running(validatorSet.keys, Set.empty, maxPower, startingBlockchain)
  }.ensuring(res => neverStuckFalse2(res))

  def notCaughtTransition(): Unit = {
    val node = SimpleNode(1)
    val nodePower: Map[Node, VotingPower] = Map((node, VotingPower(1)))
    assert(VotingPower(1) == nodePower.values.foldLeft(VotingPower(0))((acc, value) => acc + value))
    val validators = Validators(VotingPower(1), nodePower)
    val firstHeader = BlockHeader(Height(1), Set.empty, validators, validators)
    val secondHeader = BlockHeader(Height(2), Set(node), validators, validators)
    val blockchain = Blockchain(Height(3), Height(1), ChainLink(secondHeader, Genesis(firstHeader)), Set.empty)
    val blockchainState = Running(Set(node), Set.empty, VotingPower(1), blockchain)

    val faulty = Fault(node)

    assert(neverFaulty(blockchainState.step(faulty)))
  }

//  @ghost
//  def systemInvariant(blockchainState: BlockchainState, message: SystemStep): Boolean = {
//    require(blockchainState.maxHeight.value <= 3 &&
//      blockchainState.numberOfNodes == 2 &&
//      blockchainState.maxPower.value <= 1 &&
//      blockchainState.blockchain.chain.forAll(headerCheck))
//    message match {
//      case m: Messages.Fault => neverFaulty(blockchainState.step(m))
//      case m: Messages.AppendBlock => neverFaulty(blockchainState.step(m))
//      case m: Messages.TimeStep => neverFaulty(blockchainState.step(m))
//    }
//  }.holds

  @inline
  private def headerCheck(header: BlockHeader): Boolean = {
    header.height.value <= 3 && header.lastCommit.toList.size <= 2 && header.validatorSet.keys.toList.size <= 2 &&
      header.nextValidatorSet.keys.toList.size <= 2 && header.validatorSet.values.forall(value => value.value == 1) &&
      header.nextValidatorSet.values.forall(value => value.value == 1)
  }

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
  @ghost
  @inlineOnce
  def neverFaulty(blockchainState: BlockchainState): Boolean = {
    blockchainState.isInstanceOf[Running] ||
      blockchainState.isInstanceOf[Finished]
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
