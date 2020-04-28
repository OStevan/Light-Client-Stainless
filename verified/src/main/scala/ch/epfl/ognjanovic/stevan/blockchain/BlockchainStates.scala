package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.blockchain.SystemSteps._
import ch.epfl.ognjanovic.stevan.types.Nodes._
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import ch.epfl.ognjanovic.stevan.types._
import stainless.annotation._
import stainless.lang._

object BlockchainStates {

  @inlineInvariant
  sealed abstract class BlockchainState {
    @pure
    @inline
    def step(systemStep: SystemStep): BlockchainState

    def maxHeight: Height

    def numberOfNodes: BigInt

    def maxPower: VotingPower

    def blockchain: Blockchain

    def currentHeight(): Height

    def faulty: Set[Node]

    def header(height: Height): BlockHeader = {
      require(height <= blockchain.height)
      blockchain.getHeader(height)
    }

    def signedHeader(height: Height): SignedHeader = {
      require(height < blockchain.height)
      blockchain.getSignedHeader(height)
    }
  }

  @inlineInvariant
  case class Running(
    allNodes: Set[Node],
    faulty: Set[Node],
    maxVotingPower: VotingPower,
    blockchain: Blockchain) extends BlockchainState {
    require(
      allNodes.nonEmpty && // makes no sense to have no nodes
        (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
        maxVotingPower.isPositive && // makes no sense to have 0 maximum voting power
        !blockchain.finished &&
        blockchain.chain.forAll(blockHeader => blockHeader.nextValidatorSet.keys.subsetOf(allNodes)) &&
        blockchain.chain.forAll(blockHeader => blockHeader.lastCommit.subsetOf(allNodes))
    )

    private def appendBlock(lastCommit: Set[Node], nextValidatorSet: Validators): BlockchainState = {
      require(
        (lastCommit subsetOf blockchain.chain.head.validatorSet.keys) &&
          (nextValidatorSet.keys subsetOf allNodes) &&
          nextValidatorSet.keys.nonEmpty &&
          lastCommit.subsetOf(allNodes) &&
          lastCommit.nonEmpty)
      val lastBlock = blockchain.chain.head
      if (lastBlock.validatorSet.obtainedByzantineQuorum(lastCommit) && nextValidatorSet.isCorrect(faulty)) {
        val newBlockchain = blockchain.appendBlock(lastCommit, nextValidatorSet)
        if (newBlockchain.finished)
          Finished(allNodes, faulty, newBlockchain)
        else {
          assert(newBlockchain.chain.head.lastCommit == lastCommit)
          Running(allNodes, faulty, maxVotingPower, newBlockchain)
        }
      } else
        this
    }.ensuring(res => res.isInstanceOf[Running] || res.isInstanceOf[Finished])

    @pure
    @inline
    def step(systemStep: SystemStep): BlockchainState = systemStep match {
      // faultyNode is from expected nodes and at least one correct node exists
      case Fault(faultyNode) if allNodes.contains(faultyNode) && (allNodes != (faulty + faultyNode)) =>
        val newFaulty = faulty + faultyNode
        assert(newFaulty subsetOf allNodes)
        val newChain = blockchain.setFaulty(newFaulty)
        assert(newChain.chain == blockchain.chain)

        if (newChain.faultAssumption())
          Running(allNodes, newFaulty, maxVotingPower, newChain)
        else
          Faulty(allNodes, newFaulty, maxVotingPower, newChain)

      case TimeStep(step) =>
        val updated = blockchain.increaseMinTrustedHeight(step)

        if (updated.faultAssumption())
          Running(allNodes, faulty, maxVotingPower, updated)
        else
          Faulty(allNodes, faulty, maxVotingPower, updated)

      // ignores append messages which do not preserve guarantees of the system
      case AppendBlock(lastCommit, nextValidatorSet: Validators)
        if lastCommit.subsetOf(blockchain.chain.head.validatorSet.keys) &&
          nextValidatorSet.keys.subsetOf(allNodes) &&
          lastCommit.subsetOf(allNodes) =>
        appendBlock(lastCommit, nextValidatorSet)
      case _ => this // ignored cases
    }

    override def maxHeight: Height = blockchain.maxHeight

    override def maxPower: VotingPower = maxVotingPower

    override def numberOfNodes: BigInt = allNodes.toList.size

    override def currentHeight(): Height = blockchain.chain.height
  }

  @inlineInvariant
  case class Faulty(
    allNodes: Set[Node],
    faulty: Set[Node],
    maxVotingPower: VotingPower,
    blockchain: Blockchain) extends BlockchainState {
    require(
      allNodes.nonEmpty && // makes no sense to have no nodes
        (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
        maxVotingPower.isPositive && // makes no sense to have 0 maximum voting power
        !blockchain.finished &&
        blockchain.chain.forAll(blockHeader => blockHeader.nextValidatorSet.keys.subsetOf(allNodes)) &&
        blockchain.chain.forAll(blockHeader => blockHeader.lastCommit.subsetOf(allNodes))
    )

    @pure
    def step(systemStep: SystemStep): BlockchainState = (systemStep match {
      case TimeStep(step) =>
        // propagation of time allows us to move away from the chain where too many fault happened
        val updated = blockchain.increaseMinTrustedHeight(step)

        if (updated.faultAssumption())
          Running(allNodes, faulty, maxVotingPower, updated)
        else
          Faulty(allNodes, faulty, maxVotingPower, updated)

      case Fault(faultyNode) if allNodes.contains(faultyNode) && (allNodes != (faulty + faultyNode)) =>
        val newFaulty = faulty + faultyNode
        Faulty(allNodes, newFaulty, maxVotingPower, blockchain.setFaulty(newFaulty))

      case _ => this
    }).ensuring(res => res.isInstanceOf[Faulty] || res.isInstanceOf[Running])

    override def maxHeight: Height = blockchain.maxHeight

    override def maxPower: VotingPower = maxVotingPower

    override def numberOfNodes: BigInt = allNodes.toList.size

    override def currentHeight(): Height = blockchain.chain.height
  }

  @inlineInvariant
  case class Finished(allNodes: Set[Node], faulty: Set[Node], blockchain: Blockchain) extends BlockchainState {
    require(
      allNodes.nonEmpty && // makes no sense to have no nodes
        (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
        blockchain.finished &&
        blockchain.chain.forAll(blockHeader => blockHeader.nextValidatorSet.keys.subsetOf(allNodes)) &&
        blockchain.chain.forAll(blockHeader => blockHeader.lastCommit.subsetOf(allNodes))
    )

    @pure
    def step(systemStep: SystemStep): BlockchainState = this.ensuring(res => res.isInstanceOf[Finished])

    override def maxHeight: Height = blockchain.maxHeight

    override def numberOfNodes: BigInt = BigInt(0)

    override def maxPower: VotingPower = VotingPower(0)

    override def currentHeight(): Height = blockchain.chain.height
  }

}
