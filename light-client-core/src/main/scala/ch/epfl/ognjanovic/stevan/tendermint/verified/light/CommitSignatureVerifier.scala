package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

abstract class CommitSignatureVerifier {
  def verifyCommitSignatures(lightBlock: LightBlock): Boolean
}
