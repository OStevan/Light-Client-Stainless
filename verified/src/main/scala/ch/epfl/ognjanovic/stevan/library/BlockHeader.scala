package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import ch.epfl.ognjanovic.stevan.library.Nodes._

case class BlockHeader(height: Int, commit: Set[Node], validatorSet: Validators, nextValidatorSet: Validators)
