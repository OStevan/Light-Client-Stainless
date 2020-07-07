package ch.epfl.ognjanovic.stevan.tendermint.verified.fork

import stainless.annotation.{ignore, induct, opaque}
import stainless.collection._
import stainless.lang._
import stainless.lang.StaticChecks.Ensuring
import utils.{ListMap, ListSetUtils}

case class PeerList[Id, Instance](
  mapping: ListMap[Id, Instance],
  primaryId: Id,
  witnessesIds: List[Id],
  fullNodeIds: List[Id],
  faultyNodeIds: List[Id]) {
  require(
    ListOps.noDuplicate(witnessesIds) && ListOps.noDuplicate(fullNodeIds) && ListOps.noDuplicate(faultyNodeIds) &&
      PeerList.instanceInvariant(mapping, primaryId, witnessesIds, fullNodeIds, faultyNodeIds) &&
      (witnessesIds & fullNodeIds).isEmpty && (witnessesIds & faultyNodeIds).isEmpty && (fullNodeIds & faultyNodeIds).isEmpty &&
      !witnessesIds.contains(primaryId) && !faultyNodeIds.contains(primaryId) && !fullNodeIds.contains(primaryId)
  )

  def primary: Instance = mapping(primaryId)

  def witnesses: List[Instance] = {
    witnessesIds.map(witness ⇒ {
      require(mapping.contains(witness))
      mapping(witness)
    })
  }

  def markPrimaryAsFaulty: PeerList[Id, Instance] = {
    require(witnessesIds.nonEmpty)

    val newFaultyIds = primaryId :: faultyNodeIds
    val newPrimary = witnessesIds.head
    val newWitnesses = witnessesIds - witnessesIds.head
    ListSetUtils.listSetRemoveHeadSameAsSubtraction(witnessesIds)
    PeerList.removalLemma(witnessesIds.head, mapping, witnessesIds)

    if (fullNodeIds.isEmpty)
      PeerList[Id, Instance](mapping, newPrimary, newWitnesses, fullNodeIds, newFaultyIds)
    else {
      val newFullNodes = fullNodeIds - fullNodeIds.head
      ListSetUtils.listSetRemoveHeadSameAsSubtraction(fullNodeIds)
      PeerList.removalLemma(fullNodeIds.head, mapping, fullNodeIds)

      PeerList[Id, Instance](mapping, newPrimary, fullNodeIds.head :: newWitnesses, newFullNodes, newFaultyIds)
    }
  }.ensuring(res ⇒ forall((peerId: Id) ⇒ PeerList.transitionCheck(peerId, this) == PeerList.transitionCheck(peerId, res)))

  def markWitnessAsFaulty(peerId: Id): PeerList[Id, Instance] = {
    require(witnessesIds.contains(peerId))

    val newFaulty = peerId :: faultyNodeIds
    val newWitnessSet = witnessesIds - peerId
    ListSetUtils.removingFromASetResultsInASet(peerId, witnessesIds)
    PeerList.removalLemma(peerId, mapping, witnessesIds)
    PeerList.mapContainmentTransitivity(mapping, witnessesIds)
    PeerList.elementSpillLemma(peerId, mapping, faultyNodeIds)

    if (fullNodeIds.isEmpty)
      PeerList[Id, Instance](mapping, primaryId, newWitnessSet, fullNodeIds, newFaulty)
    else {
      val fullWitnessSet = fullNodeIds.head :: newWitnessSet
      val newFullNodeIds = fullNodeIds - fullNodeIds.head
      ListSetUtils.listSetRemoveHeadSameAsSubtraction(fullNodeIds)
      PeerList.removalLemma(fullNodeIds.head, mapping, fullNodeIds)

      PeerList[Id, Instance](mapping, primaryId, fullWitnessSet, newFullNodeIds, newFaulty)
    }
  }.ensuring(res ⇒ forall((peerId: Id) ⇒ PeerList.transitionCheck(peerId, this) == PeerList.transitionCheck(peerId, res)))

}

object PeerList {

  @ignore
  def fromScala[Id, Instance](
    mapping: scala.collection.Map[Id, Instance],
    primaryId: Id,
    witnessesIds: scala.List[Id],
    fullNodeIds: scala.List[Id],
    faultyNodeIds: scala.List[Id]): PeerList[Id, Instance] = {
    PeerList(
      ListMap(List.fromScala(mapping.toList)),
      primaryId,
      List.fromScala(witnessesIds),
      List.fromScala(fullNodeIds),
      List.fromScala(faultyNodeIds))
  }

  @inline
  private def instanceInvariant[Id, Instance](
    instances: ListMap[Id, Instance],
    primaryId: Id,
    witnessesIds: List[Id],
    fullNodeIds: List[Id],
    faultyNodeIds: List[Id]): Boolean = {
    // this is a less restrictive invariant which should prevent runtime errors and should be sufficient for verification
    // a stricter invariant should also say that instances contains exactly the same key as the union of all ids

    instances.contains(primaryId) && witnessesIds.forall(instances.contains) && fullNodeIds.forall(
      instances.contains) && faultyNodeIds.forall(instances.contains)
  }

  @inline
  private def transitionCheck[Id, T](peer: Id, peerList: PeerList[Id, T]): Boolean = {
    peerList.primaryId == peer ||
    peerList.witnessesIds.contains(peer) ||
    peerList.fullNodeIds.contains(peer) ||
    peerList.faultyNodeIds.contains(peer)
  }

  @opaque
  private def elementSpillLemma[T, B](element: T, map: ListMap[T, B], @induct second: List[T]): Unit = {
    require(map.contains(element) && second.forall(map.contains))
  }.ensuring(_ ⇒ (element :: second).forall(map.contains))

  @opaque
  private def removalLemma[T, B](element: T, map: ListMap[T, B], @induct list: List[T]): Unit = {
    require(list.forall(map.contains))
  }.ensuring(_ ⇒ (list - element).forall(map.contains))

  @opaque
  private def mapContainmentTransitivity[T, K](map: ListMap[T, K], @induct list: List[T]): Unit = {
    require(list.forall(map.contains))
  }.ensuring(_ ⇒ forall((elem: T) ⇒ list.contains(elem) ==> map.contains(elem)))

}
