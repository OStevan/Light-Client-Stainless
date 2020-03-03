package object test {
    sealed abstract class List
    case object Nil extends List
    case class Cons(head: BigInt, tail: List) extends List
}
