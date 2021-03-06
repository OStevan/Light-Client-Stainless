package ch.epfl.ognjanovic.stevan.tendermint.rpc

import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.Responses.{
  CommitResponse,
  JsonRpcResponse,
  StatusResponse,
  ValidatorSetResponse
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, ValidatorSet}
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import stainless.annotation.ignore
import sttp.client._
import circe.circe.{hexStringDecoder, instantDecoder}
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDecoders.{
  addressDecoder,
  keyDecoder,
  signedHeaderDecoder,
  validatorSetDecoder
}
import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.SignedHeader

@ignore
class TendermintFullNodeClient(
  private val secure: Boolean,
  private val ipAddress: String,
  private val port: Option[Int],
  private val connectionBackend: SttpBackend[Identity, Nothing, NothingT]
) {

  private val scheme = if (secure) "https" else "http"
  private val commitUri = (height: Option[Long]) => uri"$scheme://$ipAddress:$port/commit?height=$height"
  private val validatorSetUri = (height: Option[Long]) => uri"$scheme://$ipAddress:$port/validators?height=$height"
  private val statusUri = uri"$scheme://$ipAddress:$port/status"

  def commit(height: Option[Height]): SignedHeader = {
    val ACCEPTED_ENCODING = "application/json"
    val request = basicRequest.get(commitUri(height.map(_.value.toLong))).acceptEncoding(ACCEPTED_ENCODING)
    val response = connectionBackend.send(request)
    assert(response.is200, new RuntimeException("Commit request returned a non 200 error"))
    processResponse[CommitResponse](response).result.signed_header
  }

  def validatorSet(height: Option[Height]): ValidatorSet = {
    val ACCEPTED_ENCODING = "application/json"
    val request = basicRequest.get(validatorSetUri(height.map(_.value.toLong))).acceptEncoding(ACCEPTED_ENCODING)
    val response = connectionBackend.send(request)
    assert(response.is200, new RuntimeException("Commit request returned a non 200 error"))
    processResponse[ValidatorSetResponse](response).result.validators
  }

  def status(): StatusResponse = {
    val ACCEPTED_ENCODING = "application/json"
    val request = basicRequest.get(statusUri).acceptEncoding(ACCEPTED_ENCODING)
    val response = connectionBackend.send(request)
    assert(
      response.is200,
      new RuntimeException("Commit request returned a non 200 error")
    )
    processResponse[StatusResponse](response).result
  }

  private def processResponse[T](
    response: Identity[Response[Either[String, String]]]
  )(implicit decoder: Decoder[T]): JsonRpcResponse[T] = {
    (response.body match {
      case Left(value) => throw new RuntimeException(value)
      case Right(value) =>
        parse(value) match {
          case Left(value) => throw new RuntimeException(value)
          case Right(value) => value.as[JsonRpcResponse[T]]
        }
    }) match {
      case Left(value) => throw new RuntimeException(value)
      case Right(value) => value
    }
  }

}
