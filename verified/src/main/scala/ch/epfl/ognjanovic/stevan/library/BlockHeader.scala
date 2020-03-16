package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import ch.epfl.ognjanovic.stevan.library.Nodes._
import ch.epfl.ognjanovic.stevan.library.types.Height

case class BlockHeader(height: Height, lastCommit: Set[Node], validatorSet: Validators, nextValidatorSet: Validators)
