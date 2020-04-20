package utils

object StaticOps {
  @library
  implicit class StaticSetOps[A](val set: Set[A]) extends AnyVal {
    @extern
    @pure
    def staticMap[B](f: A => B): Set[B] = {
      new Set(set.theSet.map(f))
    } ensuring { res =>
      forall((a: A) => set.contains(a) == res.contains(f(a)))
    }

    @extern
    @pure
    def staticFilter(p: A => Boolean): Set[A] = {
      new Set(set.theSet.filter(p))
    } ensuring { res =>
      forall((a: A) => if (set.contains(a) && p(a)) res.contains(a) else !res.contains(a))
    }

    @extern
    @pure
    def staticToList: List[A] = {
      List.fromScala(set.theSet.toList)
    } ensuring { res =>
      forall((a: A) => res.contains(a) == set.contains(a))
    }
  }
}
