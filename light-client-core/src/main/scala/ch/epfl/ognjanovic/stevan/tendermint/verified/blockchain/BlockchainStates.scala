package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.SystemSteps.{SystemStep, _}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import stainless.annotation._
import stainless.collection.ListSet
import stainless.lang._
import stainless.lang.StaticChecks.assert

object BlockchainStates {

  @inline
  private def runningStateInvariant(
    allNodes: ListSet[Address],
    faulty: ListSet[Address],
    maxVotingPower: VotingPower,
    blockchain: Blockchain): Boolean = {
    blockchain.faultAssumption() &&
    maxVotingPower.isPositive &&
    !blockchain.finished &&
    globalStateInvariant(allNodes, faulty, blockchain)
  }

  @inline
  private def faultyStateInvariant(
    allNodes: ListSet[Address],
    faulty: ListSet[Address],
    maxVotingPower: VotingPower,
    blockchain: Blockchain): Boolean = {
    !blockchain.faultAssumption() &&
    maxVotingPower.isPositive &&
    !blockchain.finished &&
    globalStateInvariant(allNodes, faulty, blockchain)
  }

  @inline
  private def finishedStateInvariant(
    allNodes: ListSet[Address],
    faulty: ListSet[Address],
    blockchain: Blockchain): Boolean = {
    blockchain.faultAssumption() &&
    blockchain.finished &&
    globalStateInvariant(allNodes, faulty, blockchain)
  }

  // state invariant forced in TLA
  @inline
  private def globalStateInvariant(
    allNodes: ListSet[Address],
    faulty: ListSet[Address],
    blockchain: Blockchain): Boolean = {
    allNodes.nonEmpty && // makes no sense to have no nodes
    faulty.subsetOf(allNodes) && // faulty nodes need to be from the set of existing nodes
    faulty == blockchain.faulty &&
    blockchain.chain.forAll(_.nextValidatorSet.keys.subsetOf(allNodes)) &&
    blockchain.chain.forAll(_.lastCommit.forBlock.subsetOf(allNodes)) &&
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

    def faulty: ListSet[Address]

    def header(height: Height): BlockHeader = {
      require(height <= blockchain.height)
      blockchain.getHeader(height)
    }

    @pure
    def lightBlock(height: Height): LightBlock = {
      require(height < currentHeight())
      blockchain.getLightBlock(height)
    }.ensuring(res => res.header.height == height)

  }

  @inlineInvariant
  case class Running(
    allNodes: ListSet[Address],
    faulty: ListSet[Address],
    maxVotingPower: VotingPower,
    blockchain: Blockchain)
      extends BlockchainState {
    require(runningStateInvariant(allNodes, faulty, maxVotingPower, blockchain))

    @pure
    override def step(systemStep: SystemStep): BlockchainState = {
      StaticChecks.require(runningStateInvariant(allNodes, faulty, maxVotingPower, blockchain))
      systemStep match {
        // faultyNode is from expected nodes and at least one correct node exists
        case Fault(faultyNode)
            if allNodes.contains(faultyNode) && (allNodes != (faulty + faultyNode)) && !faulty.contains(faultyNode) =>
          val newFaulty = faulty + faultyNode
          val newChain = blockchain.setFaulty(newFaulty)

          if (newChain.faultAssumption())
            Running(allNodes, newFaulty, maxVotingPower, newChain)
          else
            Faulty(allNodes, newFaulty, maxVotingPower, newChain)

        case TimeStep(timeDelta) =>
          val updated = blockchain.increaseTime(timeDelta)
          Running(allNodes, faulty, maxVotingPower, updated)

        // ignores append messages which do not preserve guarantees of the system
        case AppendBlock(lastCommit, nextValidatorSet: ValidatorSet)
            if lastCommit.forBlock.subsetOf(blockchain.chain.head.validatorSet.keys) &&
              nextValidatorSet.values.forall(_.votingPower <= maxVotingPower) &&
              nextValidatorSet.keys.subsetOf(allNodes) &&
              lastCommit.forBlock.subsetOf(allNodes) &&
              lastCommit.forBlock.nonEmpty /* obvious from AppendBlock adt invariant times-out */ =>
          val lastBlock = blockchain.chain.head
          if (
            VotingPowerVerifiers.defaultVotingPowerVerifier.consensusObtained(lastBlock.validatorSet, lastCommit) &&
            blockchain.faultChecker.isCorrect(nextValidatorSet, faulty)
          ) {
            val newBlockchain = blockchain.appendBlock(lastCommit, nextValidatorSet)

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

    @pure
    override def currentHeight(): Height = blockchain.chain.height

  }

  @inlineInvariant
  case class Faulty(
    allNodes: ListSet[Address],
    faulty: ListSet[Address],
    maxVotingPower: VotingPower,
    blockchain: Blockchain)
      extends BlockchainState {
    require(faultyStateInvariant(allNodes, faulty, maxVotingPower, blockchain))

    @pure
    override def step(systemStep: SystemStep): BlockchainState = {
      StaticChecks.require(faultyStateInvariant(allNodes, faulty, maxVotingPower, blockchain))
      systemStep match {
        case TimeStep(timeDelta) =>
          // propagation of time allows us to move away from the chain where too many fault happened
          val updated = blockchain.increaseTime(timeDelta)

          if (updated.faultAssumption())
            Running(allNodes, faulty, maxVotingPower, updated)
          else
            Faulty(allNodes, faulty, maxVotingPower, updated)

        case Fault(faultyNode)
            if allNodes.contains(faultyNode) && (allNodes != (faulty + faultyNode)) && !faulty.contains(faultyNode) =>
          val newFaulty = faulty + faultyNode
          val newBlockchain = blockchain.setFaulty(newFaulty)
          Faulty(allNodes, newFaulty, maxVotingPower, newBlockchain)

        case _ => this
      }
    }.ensuring(res =>
      (res.blockchain.faultAssumption() && res.isInstanceOf[Running]) ||
        (!res.blockchain.faultAssumption() && res.isInstanceOf[Faulty]))

    override def maxHeight: Height = blockchain.maxHeight

    override def maxPower: VotingPower = maxVotingPower

    override def numberOfNodes: BigInt = allNodes.toList.size

    @pure
    override def currentHeight(): Height = blockchain.chain.height

  }

  @inlineInvariant
  case class Finished(allNodes: ListSet[Address], faulty: ListSet[Address], blockchain: Blockchain)
      extends BlockchainState {
    require(finishedStateInvariant(allNodes, faulty, blockchain))

    @pure
    override def step(systemStep: SystemStep): BlockchainState = this.ensuring(res => res.isInstanceOf[Finished])

    override def maxHeight: Height = blockchain.maxHeight

    override def numberOfNodes: BigInt = BigInt(0)

    override def maxPower: VotingPower = VotingPower(0)

    @pure
    override def currentHeight(): Height = blockchain.chain.height

  }

}
