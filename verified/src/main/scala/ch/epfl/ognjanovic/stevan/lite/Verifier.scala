package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.SignedHeader

case class Verifier(trustedState: TrustedState, blockchainClient: BlockchainClient) {

  // /**
  //  * Tries to verify the block at a given height, returning an updated verifier up to the height it managed to verify.
  //  *
  //  * @param blockToVerify given the current state
  //  * @return
  //  */
  // def verify(blockToVerify: SignedHeader): (Boolean, Verifier) = {
  //   if (blockchainClient.expired(trustedState.signedHeader))
  //     (false, this)
  //   else if (trustedState.signedHeader.header.height <= blockToVerify.header.height) {
  //     verifyBisection(blockToVerify)
  //   } else
  //     (true, Verifier(TrustedState(blockToVerify), blockchainClient))
  // }

  // private def verifyBisection(header: SignedHeader): (Boolean, Verifier) = {
  //   if (canVerify(this.trustedState.signedHeader, header)) {
  //     (true, Verifier(TrustedState(header), blockchainClient))
  //   } else if (trustedState.signedHeader.header.height + 1 == header.header.height) {
  //     (false, this)
  //   } else {
  //     val intermediateHeader = blockchainClient.loadHeader(
  //       (trustedState.signedHeader.header.height + header.header.height) / 2)
  //     val result = this.verifyBisection(intermediateHeader)
  //     if (result._1)
  //       result._2.verify(header)
  //     else
  //       (false, this)
  //   }
  // }
}
