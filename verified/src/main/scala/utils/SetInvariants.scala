package utils

import stainless.annotation.{extern, opaque}
import stainless.lang._

object SetInvariants {

  @extern @opaque
  def setAdd[T](set: Set[T], elem: T, allNodes: Set[T]): Set[T] = {
    require(allNodes.contains(elem) && set.subsetOf(allNodes))
    set + elem
  }.ensuring(res => set.subsetOf(res) && res.contains(elem) && res.subsetOf(allNodes))
}
