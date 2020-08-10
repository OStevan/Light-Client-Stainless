package utils

import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.lang.StaticChecks._

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

  def listSetSubsetEquivalence[T](set: Set[T]): List[T] = {
    val list = set.toList
    selfContainment(list)
    equivalentPredicate(list, list.contains, set.contains)
    list
  }.ensuring(res => res.forall(set.contains))

  @opaque
  def selfContainment[T](list: List[T]): Unit = {
    list match {
      case Nil() => ()
      case Cons(h, t) =>
        selfContainment(t)
        expandPredicate(t, t.contains, list.contains)
        prependMaintainsCondition(h, t, list.contains)
    }
  }.ensuring(_ => list.forall(list.contains))

  @opaque
  def expandPredicate[T](@induct list: List[T], p1: T => Boolean, p2: T => Boolean): Unit = {
    require(forall((elem: T) => p1(elem) ==> p2(elem)) && list.forall(p1))
  }.ensuring(_ => list.forall(p2))

  @opaque
  def equivalentPredicate[T](@induct list: List[T], p1: T => Boolean, p2: T => Boolean): Unit = {
    require(forall((elem: T) => p1(elem) == p2(elem)) && list.forall(p1))
  }.ensuring(_ => list.forall(p2))

  @opaque
  def subsetRemovingLemma[T](original: List[T], first: List[T], second: List[T]): Unit = {
    require(ListOps.noDuplicate(original) && first.forall(second.contains))
    decreases(original.size)
    original match {
      case Nil() => ()
      case Cons(h, t) if first.contains(h) && second.contains(h) =>
        subsetRemovingLemma(t, first, second)

      case Cons(h, t) if second.contains(h) && !first.contains(h) =>
        val removedFirst = removingFromSet(original, first)
        val removedSecond = removingFromSet(original, second)
        subsetRemovingLemma(t, first, second)
        redundantHead(removedSecond, removedFirst)

      case Cons(h, t) =>
        val removedFirst = removingFromSet(original, first)
        val removeTailSecond = removingFromSet(t, second)
        subsetRemovingLemma(t, first, second)

        transitivityOfContainment(h, first, second)
        filteringWithoutHead(original, first)
        filteringWithoutHead(original, second)

        redundantHead(removeTailSecond, removedFirst)
        prependMaintainsCondition(h, removeTailSecond, removedFirst.contains)
    }
  }.ensuring(_ => removingFromSet(original, second).forall(removingFromSet(original, first).contains))

  @opaque
  def filteringWithoutHead[T](original: List[T], @induct filter: List[T]): Unit = {
    require(ListOps.noDuplicate(original) && original.nonEmpty && !filter.contains(original.head))
  }.ensuring(_ => removingFromSet(original, filter) == original.head :: removingFromSet(original.tail, filter))

  @opaque
  def transitivityOfContainment[T](elem: T, @induct first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains) && !second.contains(elem))
  }.ensuring(_ => !first.contains(elem))

  @opaque
  def redundantHead[T](@induct first: List[T], second: List[T]): Unit = {
    require(second.nonEmpty && first.forall(second.tail.contains))
  }.ensuring(_ => first.forall(second.contains))

  @opaque
  def filteringPreservesPredicate[K, V](set: List[K], @induct list: List[(K, V)]): Unit = {
    require(ListOps.noDuplicate(list))
  }.ensuring(_ => ListOps.noDuplicate(list.filter(node => set.contains(node._1))))

  @opaque
  def subsetFilteringCreatesSubsets[K, V](first: List[K], second: List[K], assignments: List[(K, V)]): Unit = {
    require(first.forall(second.contains))
    assignments match {
      case Nil() =>
      case Cons(h, t) =>
        subsetFilteringCreatesSubsets(first, second, t)
        val secondTailFiltered = t.filter(node => second.contains(node._1))
        val secondFiltered = assignments.filter(node => second.contains(node._1))
        reflexivity(secondFiltered)
        assert(secondTailFiltered.forall(secondFiltered.contains))

        val firstTailFiltered = t.filter(node => first.contains(node._1))
        assert(firstTailFiltered.forall(secondTailFiltered.contains))

        if (!first.contains(h._1)) {
          transitivityLemma(firstTailFiltered, secondTailFiltered, secondFiltered)
        } else {
          instantiateForAll(h._1, first, second.contains)
          transitivityLemma(firstTailFiltered, secondTailFiltered, secondFiltered)
        }
    }
  }.ensuring { _ =>
    val secondFiltered = assignments.filter(node => second.contains(node._1))
    val firstFiltered = assignments.filter(node => first.contains(node._1))
    firstFiltered.forall(secondFiltered.contains)
  }

  @opaque
  def prependMaintainsCondition[T](elem: T, @induct list: List[T], p: T => Boolean): Unit = {
    require(list.forall(p) && p(elem))
  }.ensuring(_ => (elem :: list).forall(p))

  @opaque
  def instantiateForAll[T](elem: T, list: List[T], p: T => Boolean): Unit = {
    require(list.forall(p) && list.contains(elem))
    list match {
      case Nil() => ()
      case Cons(_, t) =>
        if (t.contains(elem))
          instantiateForAll(elem, t, p)
    }
  }.ensuring(_ => p(elem))

  @opaque
  def reflexivity[T](list: List[T]): Unit = {
    list match {
      case Nil() => ()
      case Cons(_, t) =>
        reflexivity(t)
        reflexivityHelper(t, list)
    }
  }.ensuring(_ => list.forall(list.contains))

  @opaque
  def reflexivityHelper[T](@induct l1: List[T], l2: List[T]): Unit = {
    require(l2.nonEmpty && l1.forall(l2.tail.contains))
  }.ensuring(_ => l1.forall(l2.contains))

  @opaque
  def transitivityLemma[T](first: List[T], second: List[T], third: List[T]): Unit = {
    require(first.forall(second.contains) && second.forall(third.contains))
    if (first.nonEmpty) {
      instantiateForAll(first.head, first, second.contains)
      instantiateForAll(first.head, second, third.contains)
      transitivityLemma(first.tail, second, third)
    }
  }.ensuring(_ => first.forall(third.contains))

  @pure
  def removingFromSet[T](@induct first: List[T], second: List[T]): List[T] = {
    require(ListOps.noDuplicate(first))
    restOfSetIsSubset(first, second)
    first -- second
  }.ensuring(res => ListOps.noDuplicate(res) && (res & second).isEmpty && res.forall(first.contains))

  @opaque
  def restOfSetIsSubset[T](first: List[T], second: List[T]): Unit = {
    require(ListOps.noDuplicate(first))
    val diff = first -- second
    first match {
      case Nil() => assert(diff.isEmpty)
      case Cons(h, t) if second.contains(h) =>
        restOfSetIsSubset(t, second)
        expandPredicate(diff, t.contains, first.contains)
      case Cons(h, t) =>
        restOfSetIsSubset(t, second)
        expandPredicate(t -- second, t.contains, first.contains)
        prependMaintainsCondition(h, t -- second, first.contains)
    }
  }.ensuring(_ => (first -- second).forall(first.contains))

  @opaque
  def setIntersectionLemma[T](first: List[T], second: List[T]): Unit = {
    require(ListOps.noDuplicate(first))
    first match {
      case Nil() => ()
      case Cons(h, t) if second.contains(h) =>
        tailSelfContained(first)

        setIntersectionLemma(t, second)
        uniqueNotAvailable(h, t & second, t)

        val tailIntersection = t & second
        transitivityLemma(tailIntersection, t, first)
        prependMaintainsCondition(h, tailIntersection, first.contains)

      case Cons(_, t) =>
        tailSelfContained(first)

        setIntersectionLemma(t, second)

        transitivityLemma(t & second, t, first)
    }
  }.ensuring { _ =>
    val intersection = first & second
    intersection.forall(first.contains) && intersection.forall(second.contains) && ListOps.noDuplicate(intersection)
  }

  @opaque
  def tailSelfContained[T](list: List[T]): Unit = {
    require(list.nonEmpty)
    list.tail match {
      case Nil() => ()
      case _: Cons[T] =>
        selfContainment(list.tail)
        redundantHead(list.tail, list)
    }
  }.ensuring(_ => list.tail.forall(list.contains))

  @opaque
  def uniqueNotAvailable[T](elem: T, @induct first: List[T], second: List[T]): Unit = {
    require(
      ListOps.noDuplicate(first) &&
        ListOps.noDuplicate(second) &&
        first.forall(second.contains) &&
        !second.contains(elem))
  }.ensuring(_ => !first.contains(elem))

  @opaque
  def intersectionContainmentLemma[T](first: List[T], second: List[T]): Unit = {
    require(ListOps.noDuplicate(first))
    setIntersectionLemma(first, second)
  }.ensuring { _ =>
    val intersection = first & second
    forall((elem: T) => intersection.contains(elem) == (first.contains(elem) && second.contains(elem)))
  }

  @opaque
  def intersectionWithSubSetsContainmentLemma[T](original: List[T], @induct first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains))
  }.ensuring(_ =>
    forall((elem: T) =>
      (original.contains(elem) && first.contains(elem)) ==> (original.contains(elem) && second.contains(elem))))

  @opaque
  def setIntersectionContainmentLemma[T](original: List[T], first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains) && ListOps.noDuplicate(original))
    setIntersectionLemma(original, first)
    setIntersectionLemma(original, second)
    intersectionWithSubSetsContainmentLemma(original, first, second)
  }.ensuring { _ =>
    val firstIntersection = original & first
    val secondIntersection = original & second
    selfContainment(firstIntersection)
    expandPredicate(firstIntersection, firstIntersection.contains, secondIntersection.contains)
    firstIntersection.forall(secondIntersection.contains)
  }

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
  def doesNotHaveHeadContainedInTail[T](@induct first: List[T], second: List[T]): Unit = {
    require(second.nonEmpty && !first.contains(second.head) && first.forall(second.contains))
  }.ensuring(_ => first.forall(second.tail.contains))

  @opaque
  def removingContainment[T](elem: T, @induct first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains))
  }.ensuring(_ => (first - elem).forall((second - elem).contains))

  @opaque
  def interestingEquality[T](elem: T, first: List[T], @induct second: List[T]): Unit = {
    require(!second.contains(elem))
  }.ensuring(_ => second -- first == second -- (first - elem))

  @opaque
  def listSetRemoveHeadSameAsSubtraction[T](list: List[T]): Unit = {
    require(list.nonEmpty && ListOps.noDuplicate(list))
    list match {
      case Cons(h, t) =>
        removingNonContained(t, h)
    }
  }.ensuring(_ => list.tail == (list - list.head))

  @opaque
  def removingNonContained[T](@induct list: List[T], elem: T): Unit = {
    require(!list.contains(elem))
  }.ensuring(_ => list == list - elem)

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
