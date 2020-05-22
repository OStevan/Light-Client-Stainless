package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import stainless.lang._
import utils.ListMap

object SignedHeaderStore {

  abstract class SignedHeaderStore {
    def put(signedHeader: SignedHeader): SignedHeaderStore = {
      ??? : SignedHeaderStore
    }.ensuring(res => res.contains(signedHeader.header.header.height))

    def get(height: Height): Option[SignedHeader]

    def contains(height: Height): Boolean
  }

  case class ListMapSignedHeaderStore(listMap: ListMap[Height, SignedHeader]) extends SignedHeaderStore {
    override def put(signedHeader: SignedHeader): SignedHeaderStore = {
      ListMapSignedHeaderStore(listMap + (signedHeader.header.header.height, signedHeader))
    }.ensuring(res => res.contains(signedHeader.header.header.height))

    override def get(height: Height): Option[SignedHeader] =
      if (contains(height))
        Some(listMap(height))
      else
        None[SignedHeader]()

    override def contains(height: Height): Boolean = listMap.contains(height)
  }

}