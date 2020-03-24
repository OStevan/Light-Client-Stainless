package ch.epfl.ognjanovic.stevan.blockchain;

import ch.epfl.ognjanovic.stevan.blockchain.BlockchainStates._
import ch.epfl.ognjanovic.stevan.blockchain.Messages.SystemStep
import stainless.annotation._
import stainless.lang._

@ghost
object BlockchainSystem {

  @ghost
  def initialSystem(): BlockchainState = Uninitialized

  @ghost
  def systemInvariant(blockchainState: BlockchainState, message: SystemStep): Boolean = {
    val newState = blockchainState.step(message)
    newState == Uninitialized || neverStuckFalse1(newState)
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
      blockchainState.isInstanceOf[Running] ||
      blockchainState.isInstanceOf[Finished]
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
