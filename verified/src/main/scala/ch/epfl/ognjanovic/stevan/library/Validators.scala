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
        require(nodes.content.toList.size == nodes.size)
        nodesPower(nodes, ZeroVotingPower) * VotingPowers.positive(3) > totalPower * VotingPowers.positive(2)
    }

    def nodesPower(nodes: List[Node], acc: VotingPower): VotingPower = {
        require(nodes.content.toList.size == nodes.size && totalPower >= acc)
        nodes match {
            case Cons(head, tail) => nodesPower(tail, acc + validators.get(head).power)
            case _ => acc
        }
    } ensuring(res => totalPower >= res)
}