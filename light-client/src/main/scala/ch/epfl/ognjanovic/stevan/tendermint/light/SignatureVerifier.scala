package ch.epfl.ognjanovic.stevan.tendermint.light

import java.security.GeneralSecurityException

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Key
import com.google.crypto.tink.subtle.Ed25519Verify

trait SignatureVerifier {
  def verify(message: Array[Byte], signature: Array[Byte]): Boolean
}

object SignatureVerifier {

  def forPublicKey(publicKey: Key): SignatureVerifier = publicKey.tpe match {
    case "tendermint/PubKeyEd25519" =>
      new Ed25519SignatureVerifier(new Ed25519Verify(publicKey.value.toArray))
    case _ => throw new IllegalArgumentException("Public key algorithm is not known:" + publicKey.tpe)
  }

  sealed private class Ed25519SignatureVerifier(private val underlyingVerifier: Ed25519Verify) extends SignatureVerifier {

    override def verify(message: Array[Byte], signature: Array[Byte]): Boolean = {
      try {
        underlyingVerifier.verify(signature, message)
        true
      } catch {
        case _: GeneralSecurityException => false
      }
    }

  }

}
