package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import ch.epfl.ognjanovic.stevan.library.types.VotingPowers._
import ch.epfl.ognjanovic.stevan.library.Nodes.Node
import ch.epfl.ognjanovic.stevan.library.types.VotingPowers

case class Validators(validators: Map[Node, PositiveVotingPower]) {
    val totalPower: VotingPower = validators.values.
      foldLeft(VotingPowers.zero())((acc, value) => acc + value)

    def obtainedByzantineQuorum(nodes: List[Node]): Boolean = {
        require(nodes.unique == nodes)
        nodesPower(nodes, ZeroVotingPower) * VotingPowers.positive(3) > totalPower * VotingPowers.positive(2)
    }

    def nodesPower(nodes: List[Node], acc: VotingPower): VotingPower = {
        require(nodes.unique == nodes && totalPower >= acc)
        nodes match {
            case Cons(head, tail) => 
              assert(tail.unique == tail)
              assert(totalPower >= acc + validators.get(head).power)

              nodesPower(tail, acc + validators.get(head).power)
            case _ => acc
        }
    } ensuring(res => totalPower >= res)
}
