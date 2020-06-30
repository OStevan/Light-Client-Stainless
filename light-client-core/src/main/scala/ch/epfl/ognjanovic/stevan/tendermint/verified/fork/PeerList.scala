package ch.epfl.ognjanovic.stevan.tendermint.verified.fork

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.PeerId
import stainless.collection._
import stainless.lang._

case class PeerList(
  instances: Map[PeerId, LightBlockProvider],
  primaryId: PeerId,
  witnessesIds: Set[PeerId],
  fullNodeIds: Set[PeerId],
  faultyNodeIds: Set[PeerId]) {
  require(
    instances.keys.toSet == (witnessesIds ++ fullNodeIds ++ faultyNodeIds + primaryId) &&
      (witnessesIds & fullNodeIds).isEmpty && (witnessesIds & faultyNodeIds).isEmpty && (fullNodeIds & faultyNodeIds).isEmpty &&
      !witnessesIds.contains(primaryId) && !faultyNodeIds.contains(primaryId) && !fullNodeIds.contains(primaryId)
  )

  def witnesses: List[(PeerId, LightBlockProvider)] = {
    witnessesIds.toList.map(witness ⇒ (witness, instances(witness)))
  }

  def primary: LightBlockProvider = instances(primaryId)

  def markPrimaryAsFaulty: PeerList = {
    require(witnessesIds.nonEmpty)
    val witnessIdsList = witnessesIds.toList
    val fullNodesIdsList = fullNodeIds.toList

    val newFaultyIds = faultyNodeIds + primaryId

    val newPrimary = witnessIdsList.head
    if (fullNodesIdsList.isEmpty)
      PeerList(instances, newPrimary, witnessIdsList.tail.toSet, fullNodeIds, newFaultyIds)
    else
      PeerList(
        instances,
        newPrimary,
        (fullNodesIdsList.head :: witnessIdsList.tail).toSet,
        fullNodesIdsList.tail.toSet,
        newFaultyIds)
  }

  def markWitnessAsFaulty(peerId: PeerId): PeerList = {
    require(witnessesIds.contains(peerId))
    val fullNodeIdList = fullNodeIds.toList
    val newFaulty = faultyNodeIds + peerId
    val newWitnessSet = witnessesIds - peerId

    assert(!fullNodeIds.contains(peerId))
    assert(!newWitnessSet.contains(peerId))
    assert(newFaulty.contains(peerId))

    if (fullNodeIdList.isEmpty)
      PeerList(instances, primaryId, newWitnessSet, fullNodeIds, newFaulty)
    else
      PeerList(instances, primaryId, newWitnessSet + fullNodeIdList.head, fullNodeIdList.tail.toSet, newFaulty)
  }

}
