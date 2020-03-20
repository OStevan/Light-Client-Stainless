package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.blockchain.Messages._
import ch.epfl.ognjanovic.stevan.types.Chain.Genesis
import ch.epfl.ognjanovic.stevan.types.Nodes._
import ch.epfl.ognjanovic.stevan.types._
import stainless.lang._
import stainless.collection._
import stainless.math._

object BlockchainStates {
  sealed abstract class BlockchainSystem {
    def step(systemStep: SystemStep): BlockchainSystem
  }

  case object Uninitialized extends BlockchainSystem {
    def step(systemStep: SystemStep): BlockchainSystem = systemStep match {
      case Initialize(validators, maxHeight, maxPower, nextValidatorSet) =>
        val genesisBlock = BlockHeader(Height(1), Set.empty, validators, nextValidatorSet)
        val startingBlockchain = Blockchain(maxHeight, Height(1), Genesis(genesisBlock), Set.empty)
        if (maxHeight.value == BigInt(1))
          Finished(startingBlockchain)
        else
          Running(validators.keys, Set.empty, maxHeight, maxPower, startingBlockchain)
      case _ => this
    }
  }

  case class Running(
                      allNodes: Set[Node],
                      faulty: Set[Node],
                      maxHeight: Height,
                      maxPower: VotingPower,
                      blockchain: Blockchain) extends BlockchainSystem {
    require(
      allNodes.nonEmpty && // makes no sense to have no nodes
        blockchain.maxHeight == maxHeight &&
        (blockchain.height.value < maxHeight.value) && // chain height must be less than the system height
        blockchain.height.value < blockchain.maxHeight.value &&
        (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
        maxPower.isPositive // makes no sense to have 0 maximum voting power
    )

    private def appendBlock(lastCommit: Set[Node], nextValidatorSet: Validators): BlockchainSystem = {
      require((lastCommit subsetOf blockchain.chain.head.validatorSet.keys) &&
        (nextValidatorSet.keys subsetOf allNodes))
      val lastBlock = blockchain.chain.head
      if (lastBlock.validatorSet.obtainedByzantineQuorum(lastCommit) && nextValidatorSet.isCorrect(faulty)) {
        val newBlockchain = blockchain.appendBlock(lastCommit, nextValidatorSet)
        if (blockchain.finished(maxHeight))
          Finished(newBlockchain)
        else {
          Running(allNodes, faulty, maxHeight, maxPower, newBlockchain)
        }
      } else
        this
    }

    def step(systemStep: SystemStep): BlockchainSystem = systemStep match {
      case _: Initialize => this
      case Fault(faultyNode) =>
        val newFaulty = faulty + faultyNode
        val newChain = blockchain.setFaulty(newFaulty)
        if (!allNodes.contains(faultyNode))
          this // ignore cases when a random node is supplied
        else if (newFaulty == allNodes)
          this // maintain at least one correct node, as per TLA spec
        else if (newChain.faultAssumption())
          Running(allNodes, newFaulty, maxHeight, maxPower, blockchain.setFaulty(newFaulty))
        else
          Faulty(allNodes, newFaulty, maxHeight, maxPower, blockchain)
      case TimeStep(step) =>
        val updated = blockchain.increaseMinTrustedHeight(step)
        assert(updated.minTrustedHeight.value <= maxHeight.value)
        if (updated.faultAssumption())
          Faulty(allNodes, faulty, maxHeight, maxPower, blockchain)
        else
          Running(allNodes, faulty, maxHeight, maxPower, updated)
      case AppendBlock(lastCommit, nextValidatorSet: Validators) =>
        // ignores append messages which do not preserve guarantees of the system
        if ((lastCommit subsetOf blockchain.chain.head.validatorSet.keys) &&
          (nextValidatorSet.keys subsetOf allNodes))
          appendBlock(lastCommit, nextValidatorSet)
        else
          this
    }
  }

  case class Faulty(
                     allNodes: Set[Node],
                     faulty: Set[Node],
                     maxHeight: Height,
                     maxPower: VotingPower,
                     blockchain: Blockchain) extends BlockchainSystem {
    require(
      allNodes.nonEmpty && // makes no sense to have no nodes
        (blockchain.height.value < maxHeight.value) && // chain height must be less than the system height
        (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
        maxPower.isPositive// makes no sense to have 0 maximum voting power
    )

    def step(systemStep: SystemStep): BlockchainSystem = systemStep match {
      case TimeStep(step) =>
        // propagation of time allows us to move away from the chain where too many fault happened
        val updated = blockchain.increaseMinTrustedHeight(step)
        if (updated.faultAssumption())
          Faulty(allNodes, faulty, maxHeight, maxPower, blockchain)
        else
          Running(allNodes, faulty, maxHeight, maxPower, updated)
      case Fault(faultyNode) =>
        val newFaulty = faulty + faultyNode
        if (!allNodes.contains(faultyNode))
          this // ignore cases when a random node is supplied
        else if (newFaulty == allNodes)
          this // maintain at least one correct node, as per TLA spec
        else // another fault can not improve the state of the chain
        Faulty(allNodes, newFaulty, maxHeight, maxPower, blockchain)
      case _ => this
    }
  }

  case class Finished(blockchain: Blockchain) extends BlockchainSystem {
    def step(systemStep: SystemStep): BlockchainSystem = this
  }
}
