package ch.epfl.ognjanovic.stevan.tendermint.verified.fork

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.PeerId
import stainless.annotation.induct
import stainless.collection._
import utils.ListMap

case class PeerList(
  instances: ListMap[PeerId, LightBlockProvider],
  primaryId: PeerId,
  witnessesIds: List[PeerId],
  fullNodeIds: List[PeerId],
  faultyNodeIds: List[PeerId]) {
  require(
    PeerList.instanceInvariant(instances, primaryId, witnessesIds, fullNodeIds, faultyNodeIds) &&
      (witnessesIds & fullNodeIds).isEmpty && (witnessesIds & faultyNodeIds).isEmpty && (fullNodeIds & faultyNodeIds).isEmpty &&
      !witnessesIds.contains(primaryId) && !faultyNodeIds.contains(primaryId) && !fullNodeIds.contains(primaryId)
  )

  def witnesses: List[(PeerId, LightBlockProvider)] = {
    witnessesIds.map(witness ⇒ (witness, instances(witness)))
  }

  def primary: LightBlockProvider = instances(primaryId)

  def markPrimaryAsFaulty: PeerList = {
    require(witnessesIds.nonEmpty)

    val newFaultyIds = faultyNodeIds :+ primaryId

    val newPrimary = witnessesIds.head
    if (fullNodeIds.isEmpty)
      PeerList(instances, newPrimary, witnessesIds.tail, fullNodeIds, newFaultyIds)
    else
      PeerList(instances, newPrimary, fullNodeIds.head :: witnessesIds.tail, fullNodeIds.tail, newFaultyIds)
  }

  def markWitnessAsFaulty(peerId: PeerId): PeerList = {
    require(witnessesIds.contains(peerId))

    assert(PeerList.instanceInvariant(instances, primaryId, witnessesIds, fullNodeIds, faultyNodeIds))
    assert(
      (witnessesIds & fullNodeIds).isEmpty && (witnessesIds & faultyNodeIds).isEmpty && (fullNodeIds & faultyNodeIds).isEmpty)
    assert(!witnessesIds.contains(primaryId) && !faultyNodeIds.contains(primaryId) && !fullNodeIds.contains(primaryId))

    val newFaulty = peerId :: faultyNodeIds
    val newWitnessSet = witnessesIds - peerId

    assert(peerId != primaryId)
    assert(!fullNodeIds.contains(peerId))
    assert(!newWitnessSet.contains(peerId))
    assert(newFaulty.contains(peerId))

    if (fullNodeIds.isEmpty) {
      PeerList.removalLemma(peerId, instances, witnessesIds)
      assert(instances.contains(peerId))
      PeerList.elementSpillLemma(peerId, instances, faultyNodeIds)
      assert(PeerList.instanceInvariant(instances, primaryId, newWitnessSet, fullNodeIds, newFaulty))

      assert(
        (newWitnessSet & fullNodeIds).isEmpty && (newWitnessSet & newFaulty).isEmpty && (fullNodeIds & newFaulty).isEmpty)

      assert(!newWitnessSet.contains(primaryId) && !newFaulty.contains(primaryId) && !fullNodeIds.contains(primaryId))

      PeerList(instances, primaryId, newWitnessSet, fullNodeIds, newFaulty)
    } else
      PeerList(instances, primaryId, newWitnessSet :+ fullNodeIds.head, fullNodeIds.tail, newFaulty)
  }

}

private object PeerList {

  @inline
  def instanceInvariant(
    instances: ListMap[PeerId, LightBlockProvider],
    primaryId: PeerId,
    witnessesIds: List[PeerId],
    fullNodeIds: List[PeerId],
    faultyNodeIds: List[PeerId]): Boolean = {
    // this is a less restrictive invariant which should prevent runtime errors and should be sufficient for verification
    // a stricter invariant should also say that instances contains exactly the same key as the union of all ids

    instances.contains(primaryId) && witnessesIds.forall(instances.contains) && fullNodeIds.forall(
      instances.contains) && faultyNodeIds.forall(instances.contains)
  }

  def elementSpillLemma[T, B](element: T, map: ListMap[T, B], @induct second: List[T]): Unit = {
    require(map.contains(element) && second.forall(map.contains))
  }.ensuring(_ ⇒ (element :: second).forall(map.contains))

  def removalLemma[T, B](element: T, map: ListMap[T, B], @induct list: List[T]): Unit = {
    require(list.forall(map.contains))
  }.ensuring(_ ⇒ (list - element).forall(map.contains))

}
