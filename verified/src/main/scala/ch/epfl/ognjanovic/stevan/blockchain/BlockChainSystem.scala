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
            case Initialize(allNodes, maxHeight, maxPower, nextValidatorSet) =>
                val initialPowerAssigments: Map[Node, VotingPower] = allNodes.toList
                    .foldLeft(Map[Node, VotingPower]())((acc, value) => acc + (value, VotingPower(1)))
                val validators = Validators(
                                    NodePowers(VotingPower(allNodes.size), initialPowerAssigments))
                val genesisBlock = BlockHeader(Height(0), Set.empty, validators, nextValidatorSet)
                val staringBlockChain = Blockchain(false, Height(0), Height(0), List(genesisBlock), Set.empty)
                if (faultAssumption(Set.empty, Height(0), staringBlockChain))
                    Faulty
                else
                    Running(allNodes, Set.empty, maxHeight, maxPower, staringBlockChain)
            case _ => this
        } 
    }

    case class Running(allNodes: Set[Node], faulty: Set[Node], maxHeight: Height, maxPower: VotingPower, blockchain: Blockchain) extends BlockchainSystem {
        require(
            allNodes.nonEmpty && // makes no sense to have no nodes
            (blockchain.height.value <= maxHeight.value) && // chain height must be less than the system height
            (faulty subsetOf allNodes) && // faulty nodes need to be from the set of existing nodes
            (maxPower.isPositive)  && // makes no sense to have 0 maximum voting power
            (blockchain.minTrustedHeight.value <= maxHeight.value) // the system needs to stop at some point
        )

        def step(systemStep: SystemStep): BlockchainSystem = systemStep match {
            case _: Initialize => this
            case TimeStep(step) => 
                val newMinTrustedHeight = Height(min(min(maxHeight.value, blockchain.height.value + 1), blockchain.minTrustedHeight.value + step))
                val newBlockchain = Blockchain(blockchain.tooManyFaults, blockchain.height, newMinTrustedHeight, blockchain.chain, blockchain.faulty)
                if (newBlockchain.chainFault())
                    Faulty()
                else
                    Running(allNodes, faulty, maxHeight, maxPower, newBlockchain)
        }
    }

    case object Faulty() extends BlockchainSystem {
        def step(systemStep: Any): BlockchainSystem = this
    }
}

