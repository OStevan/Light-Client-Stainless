package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.{Height, SignedHeader}
import stainless.lang._
import stainless.annotation._
import utils.ListMap

object SignedHeaderStore {

  abstract class SignedHeaderStore {
    def put(signedHeader: SignedHeader): SignedHeaderStore

    def get(height: Height): Option[SignedHeader]

    def contains(height: Height): Boolean

    @law
    def lawPutContains(signedHeader: SignedHeader): Boolean = {
      put(signedHeader).contains(signedHeader.header.height)
    }

    @law
    def lawPutGet(signedHeader: SignedHeader): Boolean = {
      // this law requires timeout to be increased to 60 seconds
      put(signedHeader).get(signedHeader.header.height).isDefined
    }

    @law
    def lawContainsGet(height: Height): Boolean = {
      contains(height) == get(height).isDefined
    }
  }

  case class ListMapSignedHeaderStore(listMap: ListMap[Height, SignedHeader]) extends SignedHeaderStore {
    override def put(signedHeader: SignedHeader): SignedHeaderStore =
      ListMapSignedHeaderStore(listMap + (signedHeader.header.height, signedHeader))

    override def get(height: Height): Option[SignedHeader] =
      if (listMap.contains(height))
        Some(listMap(height))
      else
        None[SignedHeader]()

    override def contains(height: Height): Boolean = listMap.contains(height)
  }

}