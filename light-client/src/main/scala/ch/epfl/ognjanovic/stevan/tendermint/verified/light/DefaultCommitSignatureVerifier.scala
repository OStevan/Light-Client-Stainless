package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import java.io.ByteArrayOutputStream
import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.light.SignatureVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{BlockId, Commit, CommitSignatures, LightBlock}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.CommitSignatures.{BlockIDFlagCommit, CommitSignature}
import com.google.protobuf.ByteString
import com.google.protobuf.timestamp.Timestamp
import tendermint.proto.types.canonical.{CanonicalBlockID, CanonicalPartSetHeader, CanonicalVote}
import tendermint.proto.types.types.SignedMsgType

sealed class DefaultCommitSignatureVerifier() extends CommitSignatureVerifier {

  override def verifyCommitSignatures(lightBlock: LightBlock): Boolean = {
    require(lightBlock.validatorSet.values.size == lightBlock.commit.signatures.size)
    lightBlock.validatorSet.values
      .zip(lightBlock.commit.signatures)
      .map(pair => (SignatureVerifier.forPublicKey(pair._1.publicKey), pair._2))
      .map(pair =>
        pair._2 match {
          case CommitSignatures.BlockIDFlagAbsent => true
          case BlockIDFlagCommit(_, _, signature) =>
            pair._1.verify(serializeVote(lightBlock.header.chainId, lightBlock.commit, pair._2), signature.toArray)
          case CommitSignatures.BlockIdFlagNil(_, _, signature) =>
            pair._1.verify(serializeVote(lightBlock.header.chainId, lightBlock.commit, pair._2), signature.toArray)
        })
      .forall(value => value)
  }

  private def convertToCanonicalBlockId(blockId: BlockId, commitSig: CommitSignature): Option[CanonicalBlockID] = {
    commitSig match {
      case _: BlockIDFlagCommit =>
        Some(
          CanonicalBlockID(
            ByteString.copyFrom(blockId.bytes.toArray),
            Some(CanonicalPartSetHeader(ByteString.copyFrom(blockId.parts.hash.toArray), blockId.parts.total))))
      case _ => None;
    }
  }

  private def convertOptionalTimestamp(timestamp: Option[Instant]): Option[Timestamp] = timestamp match {
    case None => None
    case Some(v) => Some(Timestamp(v.getEpochSecond, v.getNano))
  }

  def getTimestamp(commitSignature: CommitSignature): Option[Instant] = commitSignature match {
    case CommitSignatures.BlockIDFlagAbsent => Option.empty[Instant]
    case BlockIDFlagCommit(_, timestamp, _) => Some(timestamp)
    case CommitSignatures.BlockIdFlagNil(_, timestamp, _) => Some(timestamp)
  }

  private def convertToCanonical(str: String, commit: Commit, commitSignature: CommitSignature): CanonicalVote = {
    CanonicalVote(
      SignedMsgType.PRECOMMIT_TYPE,
      commit.height.value.toLong,
      commit.round,
      convertToCanonicalBlockId(commit.blockId, commitSignature),
      convertOptionalTimestamp(getTimestamp(commitSignature)),
      str
    )
  }

  private def serializeVote(chainId: String, commit: Commit, commitSignature: CommitSignature): Array[Byte] = {
    val outputBuffer = new ByteArrayOutputStream()
    convertToCanonical(chainId, commit, commitSignature).writeDelimitedTo(outputBuffer)
    outputBuffer.toByteArray
  }

}
