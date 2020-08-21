package utils

import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.lang.StaticChecks.{assert, require}
import stainless.proof._

/**
 * Copied as is from {@link https://github.com/epfl-lara/verifythis2020/blob/f10e38d864838f70e0bf880d48e40de8e815fbec/src/main/scala/pgp/ListUtils.scala}.
 */
@library
object ListUtils {

  def listFilterValidProp[A](@induct l: List[A], p: A => Boolean, f: A => Boolean): Unit = {
    require(l.forall(p))

  }.ensuring(_ => l.filter(f).forall(p))

  def listAppendValidProp[A](l: List[A], @induct as: List[A], p: A => Boolean): Unit = {
    require(l.forall(p) && as.forall(p))

  }.ensuring(_ => (as ++ l).forall(p))

  @opaque
  def mapPred[A, B](@induct l: List[A], f: A => B, p: B => Boolean): Unit = {
    require(l.forall(a => p(f(a))))

  }.ensuring(_ => l.map(f).forall(p))

  @opaque
  def forallContained[T](l: List[T], p: T => Boolean, x: T): Unit = {
    require(l.forall(p) && l.contains(x))

    if (!l.isEmpty && l.tail.contains(x))
      forallContained(l.tail, p, x)

  }.ensuring(_ => p(x))

  @opaque
  def subsetContained[T](l1: List[T], l2: List[T], x: T): Unit = {
    require(l1.forall(l2.contains) && l1.contains(x))

    forallContained(l1, l2.contains, x)

  }.ensuring(_ => l2.contains(x))

  def subseq[T](l1: List[T], l2: List[T]): Boolean = (l1, l2) match {
    case (Nil(), _) => true
    case (Cons(x, xs), Cons(y, ys)) =>
      (x == y && subseq(xs, ys)) ||
        subseq(l1, ys)
    case _ => false
  }

  def subseqTail[T](l1: List[T], l2: List[T]): Unit = {
    require(!l1.isEmpty && subseq(l1, l2))

    (l1, l2) match {
      case (Cons(x, xs), Cons(y, ys)) =>
        if (subseq(l1, ys))
          subseqTail(l1, ys)
        else if (!xs.isEmpty)
          subseqTail(xs, ys)
      case _ =>
        ()
    }

  }.ensuring(_ => subseq(l1.tail, l2))

  @opaque
  def subseqContains[T](l1: List[T], l2: List[T], t: T): Unit = {
    require(subseq(l1, l2) && l1.contains(t))

    (l1, l2) match {
      case (Cons(x, xs), Cons(y, ys)) =>
        if (subseq(l1, ys))
          subseqContains(l1, ys, t)
        else if (x != t)
          subseqContains(xs, ys, t)
      case _ =>
        ()
    }

  }.ensuring(_ => l2.contains(t))

  @opaque
  def subseqNotContains[T](l1: List[T], l2: List[T], t: T): Unit = {
    require(subseq(l1, l2) && !l2.contains(t))

    if (l1.contains(t))
      subseqContains(l1, l2, t)

  }.ensuring(_ => !l1.contains(t))

  @opaque
  def noDuplicateSubseq[T](l1: List[T], l2: List[T]): Unit = {
    require(subseq(l1, l2) && ListOps.noDuplicate(l2))

    (l1, l2) match {
      case (Nil(), _) =>
        ()
      case (Cons(x, xs), Cons(y, ys)) =>
        if (subseq(l1, ys)) {
          noDuplicateSubseq(l1, ys)
          check(ListOps.noDuplicate(l1))
          ()
        } else {
          assert(x == y)
          noDuplicateSubseq(xs, ys)
          assert(ListOps.noDuplicate(xs))
          assert(subseq(xs, ys))
          assert(!ys.contains(x))
          subseqNotContains(xs, ys, x)
          check(ListOps.noDuplicate(l1))
          ()
        }
      case _ =>
        ()
    }
  }.ensuring(_ => ListOps.noDuplicate(l1))

  @opaque
  def mapSubseq[A, B](l1: List[A], l2: List[A], f: A => B): Unit = {
    require(subseq(l1, l2))

    (l1, l2) match {
      case (Cons(x, xs), Cons(y, ys)) =>
        if (subseq(l1, ys))
          mapSubseq(l1, ys, f)
        else
          mapSubseq(xs, ys, f)
      case _ =>
        ()
    }

  }.ensuring(_ => subseq(l1.map(f), l2.map(f)))

  @opaque
  def filterSubseq[A](@induct l: List[A], p: A => Boolean): Unit = {}.ensuring(_ => subseq(l.filter(p), l))

  @opaque
  def noDuplicateMapFilter[A, B](l: List[A], p: A => Boolean, f: A => B): Unit = {
    require(ListOps.noDuplicate(l.map(f)))

    filterSubseq(l, p)
    mapSubseq(l.filter(p), l, f)
    noDuplicateSubseq(l.filter(p).map(f), l.map(f))

  }.ensuring(_ => ListOps.noDuplicate(l.filter(p).map(f)))

  @opaque
  def filterMapNotIn[A, B](@induct l: List[(A, B)], a: A): Unit = {}.ensuring(_ =>
    !l.filter(_._1 != a).map(_._1).contains(a))

  @opaque
  def containedTail[T](@induct l1: List[T], l2: List[T]): Unit = {
    require(!l2.isEmpty && l1.forall(l2.tail.contains))

  }.ensuring(_ => l1.forall(l2.contains))

  @opaque
  def doesNotHaveHeadContainedInTail[T](@induct first: List[T], second: List[T]): Unit = {
    require(second.nonEmpty && !first.contains(second.head) && first.forall(second.contains))
  }.ensuring(_ => first.forall(second.tail.contains))

  @opaque
  def subsetRefl[T](l: List[T]): Unit = {
    if (!l.isEmpty) {
      subsetRefl(l.tail)
      containedTail(l.tail, l)
    }
  }.ensuring(_ => l.forall(l.contains))

  @opaque
  def forallContainsSubset[T](l1: List[T], l2: List[T]): Unit = {
    require(l1.forall(l2.contains))
    if (!l1.isEmpty) {
      forallContainsSubset(l1.tail, l2) // gives us:
      assert(l1.tail.content.subsetOf(l2.content))
    }
  }.ensuring(_ => l1.content.subsetOf(l2.content))

  @opaque
  def removingNonContained[T](@induct list: List[T], elem: T): Unit = {
    require(!list.contains(elem))
  }.ensuring(_ => list == list - elem)

  @opaque
  def removingContainment[T](elem: T, @induct first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains))
  }.ensuring(_ => (first - elem).forall((second - elem).contains))

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
  def prependMaintainsCondition[T](elem: T, @induct list: List[T], p: T => Boolean): Unit = {
    require(list.forall(p) && p(elem))
  }.ensuring(_ => (elem :: list).forall(p))

  @opaque
  def nonContainedElementDoesNotInfluenceDifference[T](elem: T, first: List[T], @induct second: List[T]): Unit = {
    require(!second.contains(elem))
  }.ensuring(_ => second -- first == second -- (first - elem))

  @opaque
  def prependSubset[T](elem: T, @induct list: List[T]): Unit = {}.ensuring { _ ⇒
    ListUtils.selfContainment(list)
    val appended = elem :: list
    ListUtils.expandPredicate(list, list.contains, appended.contains)
    list.forall((elem :: list).contains)
  }

  @opaque
  def restOfSetIsSubset[T](first: List[T], second: List[T]): Unit = {
    val diff = first -- second
    first match {
      case Nil() => assert(diff.isEmpty)
      case Cons(h, t) if second.contains(h) =>
        restOfSetIsSubset(t, second)
        ListUtils.expandPredicate(diff, t.contains, first.contains)
      case Cons(h, t) =>
        restOfSetIsSubset(t, second)
        ListUtils.expandPredicate(t -- second, t.contains, first.contains)
        ListUtils.prependMaintainsCondition(h, t -- second, first.contains)
    }
  }.ensuring(_ => (first -- second).forall(first.contains))

  @pure
  def listDifference[T](@induct first: List[T], second: List[T]): List[T] = {
    restOfSetIsSubset(first, second)
    first -- second
  }.ensuring(res => (res & second).isEmpty && res.forall(first.contains))

  @opaque
  def filteringWithoutHead[T](original: List[T], @induct filter: List[T]): Unit = {
    require(original.nonEmpty && !filter.contains(original.head))
  }.ensuring(_ =>
    ListUtils.listDifference(original, filter) == original.head :: ListUtils.listDifference(original.tail, filter))

  @opaque
  def nonContainedInSuperSetNotContainedInSubset[T](elem: T, @induct first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains) && !second.contains(elem))
  }.ensuring(_ => !first.contains(elem))

  @opaque
  def removingSubsetInvertsTheRelationship[T](original: List[T], first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains))
    decreases(original.size)
    original match {
      case Nil() => ()
      case Cons(h, t) if first.contains(h) && second.contains(h) =>
        removingSubsetInvertsTheRelationship(t, first, second)

      case Cons(h, t) if second.contains(h) && !first.contains(h) =>
        val removedFirst = listDifference(original, first)
        val removedSecond = listDifference(original, second)
        removingSubsetInvertsTheRelationship(t, first, second)
        containedTail(removedSecond, removedFirst)

      case Cons(h, t) =>
        val removedFirst = listDifference(original, first)
        val removeTailSecond = listDifference(t, second)
        removingSubsetInvertsTheRelationship(t, first, second)

        nonContainedInSuperSetNotContainedInSubset(h, first, second)
        filteringWithoutHead(original, first)
        filteringWithoutHead(original, second)

        containedTail(removeTailSecond, removedFirst)
        ListUtils.prependMaintainsCondition(h, removeTailSecond, removedFirst.contains)
    }
  }.ensuring(_ => listDifference(original, second).forall(listDifference(original, first).contains))

  @opaque
  def transitivityLemma[T](first: List[T], second: List[T], third: List[T]): Unit = {
    require(first.forall(second.contains) && second.forall(third.contains))
    if (first.nonEmpty) {
      transitivePredicate(first.head, first.contains, second.contains)
      transitivePredicate(first.head, second.contains, third.contains)
      transitivityLemma(first.tail, second, third)
    }
  }.ensuring(_ => first.forall(third.contains))

  @opaque
  def tailSelfContained[T](list: List[T]): Unit = {
    require(list.nonEmpty)
    list.tail match {
      case Nil() => ()
      case _: Cons[T] =>
        ListUtils.selfContainment(list.tail)
        ListUtils.containedTail(list.tail, list)
    }
  }.ensuring(_ => list.tail.forall(list.contains))

  @opaque
  def listIntersectionLemma[T](first: List[T], second: List[T]): Unit = {
    first match {
      case Nil() => ()
      case Cons(h, t) if second.contains(h) =>
        tailSelfContained(first)

        listIntersectionLemma(t, second)

        val tailIntersection = t & second
        transitivityLemma(tailIntersection, t, first)
        ListUtils.prependMaintainsCondition(h, tailIntersection, first.contains)

      case Cons(_, t) =>
        tailSelfContained(first)

        listIntersectionLemma(t, second)

        transitivityLemma(t & second, t, first)
    }
  }.ensuring { _ =>
    val intersection = first & second
    intersection.forall(first.contains) &&
    intersection.forall(second.contains)
  }

  @opaque
  def listSubsetContainmentLemma[T](original: List[T], @induct first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains))
  }.ensuring(_ =>
    forall((elem: T) =>
      (original.contains(elem) && first.contains(elem)) ==> (original.contains(elem) && second.contains(elem))))

  @opaque
  def listSubsetIntersectionLemma[T](original: List[T], first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains))
  }.ensuring { _ =>
    ListUtils.listIntersectionLemma(original, first)
    ListUtils.listIntersectionLemma(original, second)
    listSubsetContainmentLemma(original, first, second)

    val firstIntersection = original & first
    val secondIntersection = original & second

    ListUtils.selfContainment(firstIntersection)
    ListUtils.expandPredicate(firstIntersection, firstIntersection.contains, secondIntersection.contains)
    firstIntersection.forall(secondIntersection.contains)
  }

  @opaque
  def containmentRelationship[T](elem: T, @induct first: List[T], second: List[T]): Unit = {
    require(first.forall(second.contains))
  }.ensuring(_ ⇒ first.contains(elem) ==> second.contains(elem))

  @opaque
  def transitivePredicate[T](elem: T, list: T ⇒ Boolean, p: T => Boolean): Unit = {
    require(list(elem) ==> p(elem) && list(elem))
  }.ensuring(_ => p(elem))

  @opaque
  def reflexivity[T](list: List[T]): Unit = {
    list match {
      case Nil() => ()
      case Cons(_, t) =>
        reflexivity(t)
        ListUtils.containedTail(t, list)
    }
  }.ensuring(_ => list.forall(list.contains))

  @opaque
  def filteringWithExpandingPredicateCreatesSubsets[T](first: T ⇒ Boolean, second: T ⇒ Boolean, list: List[T]): Unit = {
    require(forall((elem: T) ⇒ first(elem) ==> second(elem)))
    list match {
      case Nil() =>
      case Cons(h, t) =>
        filteringWithExpandingPredicateCreatesSubsets(first, second, t)
        val secondTailFiltered = t.filter(node => second(node))
        val secondFiltered = list.filter(node => second(node))
        ListUtils.reflexivity(secondFiltered)
        assert(secondTailFiltered.forall(secondFiltered.contains))

        val firstTailFiltered = t.filter(node => first(node))
        assert(firstTailFiltered.forall(secondTailFiltered.contains))

        if (!first(h)) {
          ListUtils.transitivityLemma(firstTailFiltered, secondTailFiltered, secondFiltered)
        } else {
          ListUtils.transitivePredicate(h, first, second)
          ListUtils.transitivityLemma(firstTailFiltered, secondTailFiltered, secondFiltered)
        }
    }
  }.ensuring { _ =>
    val secondFiltered = list.filter(node => second(node))
    val firstFiltered = list.filter(node => first(node))
    firstFiltered.forall(secondFiltered.contains)
  }

}
