package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes._
import stainless.lang._

case class BlockHeader(height: Height, lastCommit: Set[Node], validatorSet: Validators, nextValidatorSet: Validators)
