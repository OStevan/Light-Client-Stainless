package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import stainless.annotation._

object SignedHeaderProviders {

  abstract class SignedHeaderProvider {
    @pure
    def signedHeader(height: Height): SignedHeader = {
      require(height < currentHeight)
      ??? : SignedHeader
    }.ensuring(res => res.header.header.height == height)

    @pure
    def currentHeight: Height
  }

}
