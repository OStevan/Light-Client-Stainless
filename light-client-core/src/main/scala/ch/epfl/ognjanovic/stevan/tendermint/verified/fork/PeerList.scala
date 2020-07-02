package ch.epfl.ognjanovic.stevan.tendermint.verified.fork

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.PeerId
import stainless.annotation.{induct, opaque}
import stainless.collection._
import stainless.lang._
import stainless.lang.StaticChecks.Ensuring
import utils.{ListMap, ListSetUtils}

case class PeerList(
  instances: ListMap[PeerId, LightBlockProvider],
  primaryId: PeerId,
  witnessesIds: List[PeerId],
  fullNodeIds: List[PeerId],
  faultyNodeIds: List[PeerId]) {
  require(
    ListOps.noDuplicate(witnessesIds) && ListOps.noDuplicate(fullNodeIds) && ListOps.noDuplicate(faultyNodeIds) &&
      PeerList.instanceInvariant(instances, primaryId, witnessesIds, fullNodeIds, faultyNodeIds) &&
      (witnessesIds & fullNodeIds).isEmpty && (witnessesIds & faultyNodeIds).isEmpty && (fullNodeIds & faultyNodeIds).isEmpty &&
      !witnessesIds.contains(primaryId) && !faultyNodeIds.contains(primaryId) && !fullNodeIds.contains(primaryId)
  )

  def witnesses: List[LightBlockProvider] = {
    witnessesIds.map(witness ⇒ {
      require(instances.contains(witness))
      instances(witness)
    })
  }

  def primary: LightBlockProvider = instances(primaryId)

  def markPrimaryAsFaulty: PeerList = {
    require(witnessesIds.nonEmpty)

    val newFaultyIds = primaryId :: faultyNodeIds
    val newPrimary = witnessesIds.head
    val newWitnesses = witnessesIds - witnessesIds.head
    ListSetUtils.listSetRemoveHeadSameAsSubtraction(witnessesIds)
    PeerList.removalLemma(witnessesIds.head, instances, witnessesIds)

    if (fullNodeIds.isEmpty)
      PeerList(instances, newPrimary, newWitnesses, fullNodeIds, newFaultyIds)
    else {
      val newFullNodes = fullNodeIds - fullNodeIds.head
      ListSetUtils.listSetRemoveHeadSameAsSubtraction(fullNodeIds)
      PeerList.removalLemma(fullNodeIds.head, instances, fullNodeIds)

      PeerList(instances, newPrimary, fullNodeIds.head :: newWitnesses, newFullNodes, newFaultyIds)
    }
  }.ensuring(res ⇒
    forall((peerId: PeerId) ⇒ PeerList.transitionCheck(peerId, this) == PeerList.transitionCheck(peerId, res)))

  def markWitnessAsFaulty(peerId: PeerId): PeerList = {
    require(witnessesIds.contains(peerId))

    val newFaulty = peerId :: faultyNodeIds
    val newWitnessSet = witnessesIds - peerId
    ListSetUtils.removingFromASetResultsInASet(peerId, witnessesIds)
    PeerList.removalLemma(peerId, instances, witnessesIds)
    PeerList.mapContainmentTransitivity(instances, witnessesIds)
    PeerList.elementSpillLemma(peerId, instances, faultyNodeIds)

    if (fullNodeIds.isEmpty)
      PeerList(instances, primaryId, newWitnessSet, fullNodeIds, newFaulty)
    else {
      val fullWitnessSet = fullNodeIds.head :: newWitnessSet
      val newFullNodeIds = fullNodeIds - fullNodeIds.head
      ListSetUtils.listSetRemoveHeadSameAsSubtraction(fullNodeIds)
      PeerList.removalLemma(fullNodeIds.head, instances, fullNodeIds)

      PeerList(instances, primaryId, fullWitnessSet, newFullNodeIds, newFaulty)
    }
  }.ensuring(res ⇒
    forall((peerId: PeerId) ⇒ PeerList.transitionCheck(peerId, this) == PeerList.transitionCheck(peerId, res)))

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

  @inline
  def transitionCheck(peer: PeerId, peerList: PeerList): Boolean = {
    peerList.primaryId == peer ||
    peerList.witnessesIds.contains(peer) ||
    peerList.fullNodeIds.contains(peer) ||
    peerList.faultyNodeIds.contains(peer)
  }

  @opaque
  def elementSpillLemma[T, B](element: T, map: ListMap[T, B], @induct second: List[T]): Unit = {
    require(map.contains(element) && second.forall(map.contains))
  }.ensuring(_ ⇒ (element :: second).forall(map.contains))

  @opaque
  def removalLemma[T, B](element: T, map: ListMap[T, B], @induct list: List[T]): Unit = {
    require(list.forall(map.contains))
  }.ensuring(_ ⇒ (list - element).forall(map.contains))

  @opaque
  def mapContainmentTransitivity[T, K](map: ListMap[T, K], @induct list: List[T]): Unit = {
    require(list.forall(map.contains))
  }.ensuring(_ ⇒ forall((elem: T) ⇒ list.contains(elem) ==> map.contains(elem)))

}
