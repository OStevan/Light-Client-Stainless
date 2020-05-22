package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.SystemSteps.{SystemStep, _}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Nodes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{BlockHeader, Height, Validators, VotingPower}
import stainless.annotation._
import stainless.lang.StaticChecks.assert
import stainless.lang._
import utils.SetInvariants

object BlockchainStates {

  @inline
  private def runningStateInvariant(
    allNodes: Set[PeerId],
    faulty: Set[PeerId],
    maxVotingPower: VotingPower,
    blockchain: Blockchain): Boolean = {
    blockchain.faultAssumption() &&
      maxVotingPower.isPositive &&
      !blockchain.finished &&
      globalStateInvariant(allNodes, faulty, blockchain)
  }

  @inline
  private def faultyStateInvariant(
    allNodes: Set[PeerId],
    faulty: Set[PeerId],
    maxVotingPower: VotingPower,
    blockchain: Blockchain): Boolean = {
    !blockchain.faultAssumption() &&
      maxVotingPower.isPositive &&
      !blockchain.finished &&
      globalStateInvariant(allNodes, faulty, blockchain)
  }

  @inline
  private def finishedStateInvariant(allNodes: Set[PeerId], faulty: Set[PeerId], blockchain: Blockchain): Boolean = {
    blockchain.faultAssumption() &&
      blockchain.finished &&
      globalStateInvariant(allNodes, faulty, blockchain)
  }

  // state invariant forced in TLA
  @inline
  private def globalStateInvariant(allNodes: Set[PeerId], faulty: Set[PeerId], blockchain: Blockchain): Boolean = {
    allNodes.nonEmpty && // makes no sense to have no nodes
      (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
      faulty == blockchain.faulty &&
      blockchain.chain.forAll(_.nextValidatorSet.keys.subsetOf(allNodes)) &&
      blockchain.chain.forAll(_.lastCommit.subsetOf(allNodes)) &&
      blockchain.chain.forAll(_.validatorSet.keys.subsetOf(allNodes))
  }

  @inlineInvariant
  sealed abstract class BlockchainState {
    @pure
    @inline
    def step(systemStep: SystemStep): BlockchainState

    def maxHeight: Height

    def numberOfNodes: BigInt

    def maxPower: VotingPower

    def blockchain: Blockchain

    @pure
    def currentHeight(): Height

    def faulty: Set[PeerId]

    def header(height: Height): BlockHeader = {
      require(height <= blockchain.height)
      blockchain.getHeader(height)
    }

    @pure
    def signedHeader(height: Height): SignedHeader = {
      require(height < currentHeight())
      blockchain.getSignedHeader(height)
    }.ensuring(res => res.header.height == height)
  }

  @inlineInvariant
  case class Running(
    allNodes: Set[PeerId],
    faulty: Set[PeerId],
    maxVotingPower: VotingPower,
    blockchain: Blockchain) extends BlockchainState {
    require(runningStateInvariant(allNodes, faulty, maxVotingPower, blockchain))

    @pure
    override def step(systemStep: SystemStep): BlockchainState = {
      StaticChecks.require(runningStateInvariant(allNodes, faulty, maxVotingPower, blockchain))
      systemStep match {
        // faultyNode is from expected nodes and at least one correct node exists
        case Fault(faultyNode)
          if allNodes.contains(faultyNode) && (allNodes != (faulty + faultyNode)) && !faulty.contains(faultyNode) =>
          val newFaulty = faulty + faultyNode
          SetInvariants.setAdd(faulty, faultyNode, allNodes)
          val newChain = blockchain.setFaulty(newFaulty)

          if (newChain.faultAssumption())
            Running(allNodes, newFaulty, maxVotingPower, newChain)
          else
            Faulty(allNodes, newFaulty, maxVotingPower, newChain)

        case TimeStep(step) =>
          val updated = blockchain.increaseMinTrustedHeight(step)
          Running(allNodes, faulty, maxVotingPower, updated)

        // ignores append messages which do not preserve guarantees of the system
        case AppendBlock(lastCommit, nextValidatorSet: Validators)
          if lastCommit.subsetOf(blockchain.chain.head.validatorSet.keys) &&
            nextValidatorSet.values.forall(_.votingPower <= maxVotingPower) &&
            nextValidatorSet.keys.subsetOf(allNodes) &&
            lastCommit.subsetOf(allNodes) &&
            lastCommit.nonEmpty /* obvious from AppendBlock adt invariant times-out */=>

          val lastBlock = blockchain.chain.head
          if (lastBlock.validatorSet.obtainedByzantineQuorum(lastCommit) && nextValidatorSet.isCorrect(faulty)) {
            val newBlockchain = blockchain.appendBlock(lastCommit, nextValidatorSet)
            assert(newBlockchain.chain.head.validatorSet.keys.subsetOf(allNodes))
            assert(globalStateInvariant(allNodes, faulty, newBlockchain))

            if (newBlockchain.finished)
              Finished(allNodes, faulty, newBlockchain)
            else
              Running(allNodes, faulty, maxVotingPower, newBlockchain)
          } else
            this
        case _ => this // ignored cases
      }
    }

    override def maxHeight: Height = blockchain.maxHeight

    override def maxPower: VotingPower = maxVotingPower

    override def numberOfNodes: BigInt = allNodes.toList.size

    override def currentHeight(): Height = blockchain.chain.height
  }

  @inlineInvariant
  case class Faulty(
    allNodes: Set[PeerId],
    faulty: Set[PeerId],
    maxVotingPower: VotingPower,
    blockchain: Blockchain) extends BlockchainState {
    require(faultyStateInvariant(allNodes, faulty, maxVotingPower, blockchain))

    @pure
    override def step(systemStep: SystemStep): BlockchainState = {
      StaticChecks.require(faultyStateInvariant(allNodes, faulty, maxVotingPower, blockchain))
      systemStep match {
        case TimeStep(step) =>
          // propagation of time allows us to move away from the chain where too many fault happened
          val updated = blockchain.increaseMinTrustedHeight(step)

          if (updated.faultAssumption())
            Running(allNodes, faulty, maxVotingPower, updated)
          else
            Faulty(allNodes, faulty, maxVotingPower, updated)

        case Fault(faultyNode)
          if allNodes.contains(faultyNode) && (allNodes != (faulty + faultyNode)) && !faulty.contains(faultyNode) =>
          val newFaulty = SetInvariants.setAdd(faulty, faultyNode, allNodes)
          Faulty(allNodes, newFaulty, maxVotingPower, blockchain.setFaulty(newFaulty))

        case _ => this
      }
    }.ensuring(res => (res.blockchain.faultAssumption() && res.isInstanceOf[Running]) ||
      (!res.blockchain.faultAssumption() && res.isInstanceOf[Faulty]))

    override def maxHeight: Height = blockchain.maxHeight

    override def maxPower: VotingPower = maxVotingPower

    override def numberOfNodes: BigInt = allNodes.toList.size

    override def currentHeight(): Height = blockchain.chain.height
  }

  @inlineInvariant
  case class Finished(allNodes: Set[PeerId], faulty: Set[PeerId], blockchain: Blockchain) extends BlockchainState {
    require(finishedStateInvariant(allNodes, faulty, blockchain))

    @pure
    override def step(systemStep: SystemStep): BlockchainState = this.ensuring(res => res.isInstanceOf[Finished])

    override def maxHeight: Height = blockchain.maxHeight

    override def numberOfNodes: BigInt = BigInt(0)

    override def maxPower: VotingPower = VotingPower(0)

    override def currentHeight(): Height = blockchain.chain.height
  }

}
