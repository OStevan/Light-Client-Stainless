package ch.epfl.ognjanovic.stevan.blockchain

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import stainless.math._
import ch.epfl.ognjanovic.stevan.library.Nodes._
import ch.epfl.ognjanovic.stevan.library.types._
import ch.epfl.ognjanovic.stevan.library._
import ch.epfl.ognjanovic.stevan.blockchain.Messages._

object BlockchainStates {

    def faultAssumption(faulty: Set[Node], minTrustedHeight: Height, blockchain: Blockchain): Boolean = {
        val chain = blockchain.chain
        chain.slice(min(minTrustedHeight.value, chain.length), chain.length).forall(header => header.nextValidatorSet.isCorrect(faulty))
    }

    sealed abstract class BlockchainSystem {
        def step(systemStep: SystemStep): BlockchainSystem
    }

    case object Uninitialized extends BlockchainSystem {
        def step(systemStep: SystemStep): BlockchainSystem = systemStep match {
            case Initialize(nodePowers, maxHeight, maxPower, nextValidatorSet) =>
                val validators = Validators(nodePowers)
                val genesisBlock = BlockHeader(Height(0), Set.empty, validators, nextValidatorSet)
                val staringBlockChain = Blockchain(false, Height(0), Height(0), List(genesisBlock), Set.empty)
                if (maxHeight.value == BigInt(0))
                    Finished(staringBlockChain)
                else
                    Running(nodePowers.keys, Set.empty, maxHeight, maxPower, staringBlockChain)
            case _ => this
        } 
    }

    case class Running(allNodes: Set[Node], faulty: Set[Node], maxHeight: Height, maxPower: VotingPower, blockchain: Blockchain) extends BlockchainSystem {
        require(
            allNodes.nonEmpty && // makes no sense to have no nodes
            (blockchain.height.value < maxHeight.value) && // chain height must be less than the system height
            (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
            (maxPower.isPositive)  && // makes no sense to have 0 maximum voting power
            (blockchain.minTrustedHeight.value <= maxHeight.value) // the system needs to stop at some point
        )

        private def appendBlock(lastCommit: Set[Node], nodePowers: NodePowers) = {
            require((lastCommit subsetOf blockchain.chain.head.validatorSet.validators.keys) &&
                (nodePowers.keys subsetOf allNodes))
            val lastBlock = blockchain.chain.head
            val nextVP = Validators(nodePowers)
            if (lastBlock.validatorSet.obtainedByzantineQuorum(lastCommit) && nextVP.isCorrect(faulty)) {
                val newBlockchain = blockchain.appendBlock(lastCommit, lastBlock.nextValidatorSet, nextVP)
                if (blockchain.oneMore(maxHeight))
                    Finished(newBlockchain)
                else
                    Running(allNodes, faulty, maxHeight, maxPower, newBlockchain)
            } else
                this
        }

        def step(systemStep: SystemStep): BlockchainSystem = systemStep match {
            case _: Initialize => this
            case Fault(faultyNode) => 
                val newFaulty = faulty + faultyNode
                if (!allNodes.contains(faultyNode))
                    this // ignore cases when a random node is supplied
                else if (newFaulty == allNodes)
                    this // maintain at least one correct node
                else if (faultAssumption(newFaulty, blockchain.minTrustedHeight, blockchain))
                    Running(allNodes, newFaulty, maxHeight, maxPower, blockchain.setFaulty(newFaulty))
                else 
                    Faulty
            case TimeStep(step) => 
                val newMinTrustedHeight = Height(min(min(maxHeight.value, blockchain.height.value + 1), blockchain.minTrustedHeight.value + step))
                val newBlockchain = Blockchain(blockchain.tooManyFaults, blockchain.height, newMinTrustedHeight, blockchain.chain, blockchain.faulty)
                if (newBlockchain.chainFault())
                    Faulty
                else
                    Running(allNodes, faulty, maxHeight, maxPower, newBlockchain)
            case AppendBlock(lastCommit, nodePowers) => 
                appendBlock(lastCommit, nodePowers)
        }
    }

    case object Faulty extends BlockchainSystem {
        def step(systemStep: SystemStep): BlockchainSystem = this
    }

    case class Finished(blockchain: Blockchain) extends BlockchainSystem {
        def step(systemStep: SystemStep): BlockchainSystem = this
    }
}

