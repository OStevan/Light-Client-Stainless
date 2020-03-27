package ch.epfl.ognjanovic.stevan.utils
import stainless.collection._
import stainless.annotation._
import stainless.lang._
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
  def mapPred[A,B](@induct l: List[A], f: A => B, p: B => Boolean): Unit = {
    require(l.forall(a => p(f(a))))

  }.ensuring(_ => l.map(f).forall(p))

  @opaque
  def subsetContains[T](@induct l1: List[T], l2: List[T]): Unit = {
    require(l1.content.subsetOf(l2.content))

  }.ensuring(_ => l1.forall(l2.contains))

  def noDuplicate[T](l: List[T]): Boolean = l match {
    case Nil() => true
    case Cons(x, xs) => !xs.contains(x) && noDuplicate(xs)
  }

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

  }.ensuring(_ =>
    l2.contains(x)
  )

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

  }.ensuring(_ =>
    subseq(l1.tail, l2)
  )

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

  }.ensuring(_ =>
    l2.contains(t)
  )

  @opaque
  def subseqNotContains[T](l1: List[T], l2: List[T], t: T): Unit = {
    require(subseq(l1, l2) && !l2.contains(t))

    if (l1.contains(t))
      subseqContains(l1, l2, t)

  }.ensuring(_ =>
    !l1.contains(t)
  )

  @opaque
  def noDuplicateSubseq[T](l1: List[T], l2: List[T]): Unit = {
    require(subseq(l1, l2) && noDuplicate(l2))

    (l1, l2) match {
      case (Nil(), _) =>
        ()
      case (Cons(x, xs), Cons(y, ys)) =>
        if (subseq(l1, ys)) {
          noDuplicateSubseq(l1, ys)
          check(noDuplicate(l1))
          ()
        } else {
          assert(x == y)
          noDuplicateSubseq(xs, ys)
          assert(noDuplicate(xs))
          assert(subseq(xs, ys))
          assert(!ys.contains(x))
          subseqNotContains(xs, ys, x)
          check(noDuplicate(l1))
          ()
        }
      case _ =>
        ()
    }
  }.ensuring(_ =>
    noDuplicate(l1)
  )

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

  }.ensuring(subseq(l1.map(f), l2.map(f)))

  @opaque
  def filterSubseq[A](@induct l: List[A], p: A => Boolean): Unit = {

  }.ensuring(subseq(l.filter(p), l))

  @opaque
  def noDuplicateMapFilter[A, B](l: List[A], p: A => Boolean, f: A => B): Unit = {
    require(noDuplicate(l.map(f)))

    filterSubseq(l, p)
    mapSubseq(l.filter(p), l, f)
    noDuplicateSubseq(l.filter(p).map(f), l.map(f))

  }.ensuring(_ =>
    noDuplicate(l.filter(p).map(f))
  )

  @opaque
  def filterMapNotIn[A, B](@induct l: List[(A, B)], a: A): Unit = {

  }.ensuring(_ =>
    !l.filter(_._1 != a).map(_._1).contains(a)
  )

  @opaque
  def containedTail[T](@induct l1: List[T], l2: List[T]): Unit = {
    require(!l2.isEmpty && l1.forall(l2.tail.contains))

  }.ensuring(_ =>
    l1.forall(l2.contains)
  )

  @opaque
  def subsetRefl[T](l: List[T]): Unit = {
    if (!l.isEmpty) {
      subsetRefl(l.tail)
      containedTail(l.tail, l)
    }
  }.ensuring(_ =>
    l.forall(l.contains)
  )

  @opaque
  def forallContainsSubset[T](l1: List[T], l2: List[T]): Unit = {
    require(l1.forall(l2.contains))
    if (!l1.isEmpty) {
      forallContainsSubset(l1.tail, l2) // gives us:
      assert(l1.tail.content.subsetOf(l2.content))
    }
  }.ensuring(_ =>
    l1.content.subsetOf(l2.content)
  )
}
