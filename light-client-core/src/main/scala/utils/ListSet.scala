package utils

import stainless.annotation.{library, opaque}
import stainless.collection.{ListOps, _}
import stainless.lang._
import stainless.lang.StaticChecks.require

@library
case class ListSet[T](toList: List[T]) {
  require(ListOps.noDuplicate(toList))

  def +(elem: T): ListSet[T] = {
    if (toList.contains(elem)) {
      ListUtils.selfContainment(toList)
      this
    } else {
      ListUtils.prependSubset(elem, toList)
      ListSet(elem :: toList)
    }
  }.ensuring(res ⇒ res.contains(elem) && this.subsetOf(res))

  def ++(other: ListSet[T]): ListSet[T] = {
    val union = ListSetUtils.removeDuplicates(this.toList ++ other.toList)
    ListSet(union)
  }.ensuring(res ⇒ forall((elem: T) ⇒ (this.contains(elem) || other.contains(elem)) == res.contains(elem)))

  def -(elem: T): ListSet[T] = {
    ListSetUtils.removingFromASetResultsInASet(elem, toList)
    ListSet(toList - elem)
  }.ensuring(res ⇒ !res.contains(elem))

  def --(other: ListSet[T]): ListSet[T] = {
    ListSetUtils.listSetDiff(toList, other.toList)
    ListUtils.restOfSetIsSubset(toList, other.toList)
    ListSet(toList -- other.toList)
  }.ensuring(res ⇒
    forall((elem: T) ⇒ (this.contains(elem) && !other.contains(elem)) == res.contains(elem)) &&
      (res & other).isEmpty &&
      res.subsetOf(this))

  def &(other: ListSet[T]): ListSet[T] = {
    ListSetUtils.listSetIntersection(toList, other.toList)
    ListUtils.listIntersectionLemma(toList, other.toList)
    ListSet(toList & other.toList)
  }.ensuring(res ⇒
    forall((elem: T) ⇒ (this.contains(elem) && other.contains(elem)) == res.contains(elem)) &&
      res.subsetOf(this) &&
      res.subsetOf(other))

  def filter(predicate: T ⇒ Boolean): ListSet[T] = {
    ListSetUtils.filteringPreservesPredicate(toList, predicate)
    ListSetUtils.filteringMakesSubset(toList, predicate)
    ListSet(toList.filter(predicate))
  }.ensuring(res ⇒ res.subsetOf(this))

  def size: BigInt = toList.size
  def isEmpty: Boolean = toList.isEmpty
  def nonEmpty: Boolean = toList.nonEmpty

  def contains(elem: T): Boolean = toList.contains(elem)
  def subsetOf(other: ListSet[T]): Boolean = toList.forall(other.toList.contains)
}

@library
object ListSet {
  def empty[T]: ListSet[T] = ListSet(List.empty[T])

  @library
  object lemmas {

    @opaque
    def subsetRemovalLemma[T](original: ListSet[T], first: ListSet[T], second: ListSet[T]): Unit = {
      require(first.subsetOf(second))
      ListUtils.removingSubsetInvertsTheRelationship(original.toList, first.toList, second.toList)
    }.ensuring(_ => (original -- second).subsetOf(original -- first))

    @opaque
    def containmentTransitivity[T](first: ListSet[T], second: ListSet[T], third: ListSet[T]): Unit = {
      require(first.subsetOf(second) && second.subsetOf(third))
      ListUtils.transitivityLemma(first.toList, second.toList, third.toList)
    }.ensuring(_ ⇒ first.subsetOf(third))

    @opaque
    def selfContained[T](listSet: ListSet[T]): Unit = {
      ListUtils.reflexivity(listSet.toList)
    }.ensuring(_ ⇒ listSet.subsetOf(listSet))

  }

}
