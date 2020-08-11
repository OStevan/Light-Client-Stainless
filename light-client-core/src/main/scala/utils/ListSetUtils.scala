package utils

import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.lang.StaticChecks._

@library
object ListSetUtils {

  @opaque
  def uniquenessTransitivity[A, B](list: List[(A, B)]): Unit = {
    require(ListOps.noDuplicate(list.map(_._1)))
    list match {
      case Nil() => ()
      case Cons(h, t) =>
        uniquenessTransitivity(t)
        pairUniquenessOnFirstElementLemma(h, t)
    }
  }.ensuring(_ => ListOps.noDuplicate(list))

  @opaque
  def pairUniquenessOnFirstElementLemma[A, B](elem: (A, B), @induct list: List[(A, B)]): Unit = {
    require(!list.map(_._1).contains(elem._1) && ListOps.noDuplicate(list))
  }.ensuring(_ => !list.contains(elem))

  @opaque
  def filteringPreservesPredicate[T](@induct list: List[T], predicate: T ⇒ Boolean): Unit = {
    require(ListOps.noDuplicate(list))
  }.ensuring(_ => ListOps.noDuplicate(list.filter(predicate)))

  @opaque
  def filteringMakesSubset[T](list: List[T], predicate: T ⇒ Boolean): Unit = {
    list match {
      case Cons(_, t) ⇒
        filteringMakesSubset(t, predicate)
        ListUtils.tailSelfContained(list)
        ListUtils.transitivityLemma(t.filter(predicate), t, list)
      case Nil() ⇒ ()
    }
  }.ensuring(_ => list.filter(predicate).forall(list.contains))

  @opaque
  def subsetFilteringCreatesSubsets[K, V](first: List[K], second: List[K], assignments: List[(K, V)]): Unit = {
    require(first.forall(second.contains))
    assignments match {
      case Nil() =>
      case Cons(h, t) =>
        subsetFilteringCreatesSubsets(first, second, t)
        val secondTailFiltered = t.filter(node => second.contains(node._1))
        val secondFiltered = assignments.filter(node => second.contains(node._1))
        ListUtils.reflexivity(secondFiltered)
        assert(secondTailFiltered.forall(secondFiltered.contains))

        val firstTailFiltered = t.filter(node => first.contains(node._1))
        assert(firstTailFiltered.forall(secondTailFiltered.contains))

        if (!first.contains(h._1)) {
          ListUtils.transitivityLemma(firstTailFiltered, secondTailFiltered, secondFiltered)
        } else {
          ListUtils.containmentRelationship(h._1, first, second)

          ListUtils.transitivePredicate(h._1, first.contains, second.contains)
          ListUtils.transitivityLemma(firstTailFiltered, secondTailFiltered, secondFiltered)
        }
    }
  }.ensuring { _ =>
    val secondFiltered = assignments.filter(node => second.contains(node._1))
    val firstFiltered = assignments.filter(node => first.contains(node._1))
    firstFiltered.forall(secondFiltered.contains)
  }

  @pure
  def removingFromSet[T](@induct first: List[T], second: List[T]): List[T] = {
    require(ListOps.noDuplicate(first))
    ListUtils.restOfSetIsSubset(first, second)
    first -- second
  }.ensuring(res => ListOps.noDuplicate(res) && (res & second).isEmpty && res.forall(first.contains))

  @opaque
  def removingDifference[T](first: List[T], second: List[T]): Unit = {
    require(first.nonEmpty && ListOps.noDuplicate(first) && first.forall(second.contains))
    first match {
      case Cons(h, Nil()) =>
        assert((second -- first.tail).contains(h))
      case Cons(h, _) =>
        assert((second -- first.tail).contains(h))
    }
  }.ensuring(_ => (second -- first.tail).contains(first.head))

  @opaque
  def listSetRemoveHeadSameAsSubtraction[T](list: List[T]): Unit = {
    require(list.nonEmpty && ListOps.noDuplicate(list))
    list match {
      case Cons(h, t) =>
        ListUtils.removingNonContained(t, h)
    }
  }.ensuring(_ => list.tail == (list - list.head))

  @opaque
  def removingFromASetResultsInASet[T](elem: T, @induct list: List[T]): Unit = {
    require(ListOps.noDuplicate(list))
  }.ensuring(_ ⇒ ListOps.noDuplicate(list - elem))

  @opaque
  def removeDuplicates[T](list: List[T]): List[T] = {
    list match {
      case Cons(h, t) ⇒ if (t.contains(h)) removeDuplicates(t) else h :: removeDuplicates(t)
      case Nil() ⇒ Nil[T]()
    }
  }.ensuring(res ⇒ ListOps.noDuplicate(res) && forall((elem: T) ⇒ list.contains(elem) == res.contains(elem)))

  @opaque
  def listSetDiff[T](@induct first: List[T], second: List[T]): Unit = {
    require(ListOps.noDuplicate(first) && ListOps.noDuplicate(second))
  }.ensuring(_ ⇒ ListOps.noDuplicate(first -- second))

  @opaque
  def listSetIntersection[T](@induct first: List[T], second: List[T]): Unit = {
    require(ListOps.noDuplicate(first) && ListOps.noDuplicate(second))
  }.ensuring(_ ⇒ ListOps.noDuplicate(first & second))

}
