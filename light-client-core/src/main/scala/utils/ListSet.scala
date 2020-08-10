package utils

import stainless.collection._
import stainless.lang._

case class ListSet[T](toList: List[T]) {
  require(ListOps.noDuplicate(toList))

  def +(elem: T): ListSet[T] = {
    if (toList.contains(elem)) {
      ListSetUtils.selfContainment(toList)
      this
    } else {
      ListSetUtils.prependSubset(elem, toList)
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
    ListSet(toList -- other.toList)
  }.ensuring(res ⇒ forall((elem: T) ⇒ (this.contains(elem) && !other.contains(elem)) == res.contains(elem)))

  def &(other: ListSet[T]): ListSet[T] = {
    ListSetUtils.setIntersectionLemma(toList, other.toList)
    ListSetUtils.intersectionContainmentLemma(toList, other.toList)
    ListSet(toList & other.toList)
  }.ensuring(res ⇒
    forall((elem: T) ⇒ (this.contains(elem) && other.contains(elem)) == res.contains(elem)) && res.subsetOf(this) && res
      .subsetOf(other))

  def size: BigInt = toList.size
  def isEmpty: Boolean = toList.isEmpty
  def nonEmpty: Boolean = toList.nonEmpty

  def contains(elem: T): Boolean = toList.contains(elem)
  def subsetOf(other: ListSet[T]): Boolean = toList.forall(other.toList.contains)
}

object ListSet {
  def empty[T]: ListSet[T] = ListSet(List.empty[T])
}
