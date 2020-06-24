package ch.epfl.ognjanovic.stevan.tendermint.rpc

import ch.epfl.ognjanovic.stevan.tendermint.rpc.Responses.{CommitResponse, JsonRpcResponse, ValidatorSetResponse}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, ValidatorSet}
import io.circe.generic.auto._
import io.circe.parser._
import stainless.annotation.ignore
import sttp.client._

@ignore
class TendermintFullNodeClient(

  private val secure: Boolean,
  private val ipAddress: String,
  private val port: Option[Int]) {
  private val connectionBackend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()
  private val scheme = if (secure) "https" else "http"
  private val commitUri = (height: Option[Long]) => uri"$scheme://$ipAddress:$port/commit?height=$height"
  private val validatorSetUri = (height: Option[Long]) => uri"$scheme://$ipAddress:$port/validators?height=$height"

  def commit(height: Option[Height]): SignedHeader = {
    val ACCEPTED_ENCODING = "application/json"
    val request = basicRequest.get(commitUri(height.map(_.value.toLong))).acceptEncoding(ACCEPTED_ENCODING)
    val response = connectionBackend.send(request)
    assert(response.is200, new RuntimeException("Commit request returned a non 200 error"))
    val signedHeader = (response.body match {
      case Left(value) => throw new RuntimeException(value)
      case Right(value) =>
        parse(value) match {
        case Left(value) => throw new RuntimeException(value)
        case Right(value) => value.as[JsonRpcResponse[CommitResponse]]
      }
    }) match {
      case Left(value) => throw new RuntimeException(value)
      case Right(value) => value
    }
    signedHeader.result.signed_header
  }

  def validatorSet(height:Option[Height]): ValidatorSet = {
    val ACCEPTED_ENCODING = "application/json"
    val request = basicRequest.get(validatorSetUri(height.map(_.value.toLong))).acceptEncoding(ACCEPTED_ENCODING)
    val response = connectionBackend.send(request)
    assert(response.is200, new RuntimeException("Commit request returned a non 200 error"))
    val signedHeader = (response.body match {
      case Left(value) => throw new RuntimeException(value)
      case Right(value) =>
        parse(value) match {
        case Left(value) => throw new RuntimeException(value)
        case Right(value) => value.as[JsonRpcResponse[ValidatorSetResponse]]
      }
    }) match {
      case Left(value) => throw new RuntimeException(value)
      case Right(value) => value
    }
    signedHeader.result.validators
  }
}
