package ch.epfl.ognjanovic.stevan.library

import ch.epfl.ognjanovic.stevan.library.Nodes._

case class BlockHeader(height: Height, lastCommit: Set[Node], validatorSet: Validators, nextValidatorSet: Validators)
