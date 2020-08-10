package utils

import stainless.collection._
import stainless.lang._

case class ListSet[T](toList: List[T]) {
  require(ListOps.noDuplicate(toList))

  def +(elem: T): ListSet[T] = {
    if (toList.contains(elem))
      this
    else {
      ListSet(elem :: toList)
    }
  }.ensuring(res ⇒ res.contains(elem))

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
    ListSetUtils.listSetIntersection(toList, other.toList)
    ListSet(toList & other.toList)
  }.ensuring(res ⇒ forall((elem: T) ⇒ (this.contains(elem) && other.contains(elem)) == res.contains(elem)))

  def size: BigInt = toList.size
  def isEmpty: Boolean = toList.isEmpty

  def contains(elem: T): Boolean = toList.contains(elem)
  def subsetOf(other: ListSet[T]): Boolean = toList.forall(other.toList.contains)
}
