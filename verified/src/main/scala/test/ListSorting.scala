package test

import stainless.lang.Set

object ListSorting {
    def size(list: List): BigInt = (list match {
        case Nil => BigInt(0)
        case Cons(x, rest) => 1 + size(rest)
    }) ensuring(res => res >= 0)

    def isSorted(list: List): Boolean = list match {
        case Nil => true
        case Cons(head, Nil) => true
        case Cons(x, Cons(y, tail)) => x <= y && isSorted(Cons(y, tail)) 
    }

    def content(l: List): Set[BigInt] = l match {
        case Nil => Set()
        case Cons(i, t) => Set(i) ++ content(t)
    }

    def insertSorted(value: BigInt, list: List): List = {
        require(isSorted(list))
        list match {
            case Nil => Cons(value, Nil)
            case Cons(head, tail) if head == value => list 
            case Cons(head, tail) if head > value => Cons(value, list)
            case Cons(head, tail) => Cons(head, insertSorted(value, tail))
        }
    }ensuring(res => isSorted(res) && content(res) == content(list) ++ Set(value))
}
