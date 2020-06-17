package ch.epfl.ognjanovic.stevan.tendermint.hashing

import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.ByteArray
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Validators.InfoHashable
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Header, Key, ValidatorSet}
import com.google.protobuf.timestamp.Timestamp
import com.google.protobuf.{ByteString, CodedOutputStream}
import tendermint.proto.types.types.{BlockID, PartSetHeader}
import tendermint.proto.version.version.Consensus

object Hashers {
  trait Hasher {
    def hashHeader(header: Header): ByteArray
    def hashValidatorSet(validatorSet: ValidatorSet): ByteArray
  }

  sealed class DefaultHasher(private val merkleRoot: MerkleRoot) extends Hasher {
    override def hashHeader(header: Header): ByteArray = {
      val fieldsBytes = Array[Array[Byte]](
        Consensus(header.version.block, header.version.app).toByteArray,
        encodeByteVector(header.chainId.getBytes),
        encodeVarInt(header.height.value.toLong),
        Timestamp(header.time.getEpochSecond, header.time.getNano).toByteArray,
        extractBlockID(header).toByteArray,
        encodeByteVector(header.lastCommit),
        encodeByteVector(header.data),
        encodeByteVector(header.validators),
        encodeByteVector(header.nextValidators),
        encodeByteVector(header.consensus),
        encodeByteVector(header.app),
        encodeByteVector(header.lastResults),
        encodeByteVector(header.evidence),
        encodeByteVector(header.proposer.address)
      )

      merkleRoot.computeRoot(fieldsBytes).toVector
    }

    private def extractBlockID(header: Header): BlockID = {
      BlockID(
        ByteString.copyFrom(header.lastBlockId.bytes.toArray),
        Some(
          PartSetHeader(
            header.lastBlockId.parts.total,
            ByteString.copyFrom(header.lastBlockId.parts.hash.toArray))))
    }

    private def encodeByteVector(bytes: ByteArray): Array[Byte] = {
      if (bytes.isEmpty)
        return Array.empty
      val byteString = ByteString.copyFrom(bytes.toArray)
      val output = Array.ofDim[Byte](CodedOutputStream.computeBytesSizeNoTag(byteString))
      val outputBuffer = CodedOutputStream.newInstance(output)
      outputBuffer.writeBytesNoTag(byteString)
      output
    }

    private def encodeVarInt(long: Long): Array[Byte] = {
      val output = Array.ofDim[Byte](CodedOutputStream.computeUInt64SizeNoTag(long))
      val outputBuffer = CodedOutputStream.newInstance(output)
      outputBuffer.writeUInt64NoTag(long)
      output
    }

    override def hashValidatorSet(validatorSet: ValidatorSet): ByteArray = {
      val validatorBytes = validatorSet.values.toScala
        .map(_.toInfoHashable)
        .map(encodeValidator)
        .toArray

      merkleRoot.computeRoot(validatorBytes).toVector
    }

    private def encodeValidator(hashable: InfoHashable): Array[Byte] = {
      val publicKeyAminoEncoded = encodePublicKey(hashable.publicKey)
      val votingPowerAsLong = hashable.votingPower.power().toLong
      val output = Array.ofDim[Byte](
        CodedOutputStream.computeByteArraySize(1, publicKeyAminoEncoded) +
        CodedOutputStream.computeUInt64Size(2, votingPowerAsLong))
      val outputBuffer = CodedOutputStream.newInstance(output)
      outputBuffer.writeByteArray(1, publicKeyAminoEncoded)
      outputBuffer.writeUInt64(2, votingPowerAsLong)
      output
    }

    // https://docs.tendermint.com/master/spec/blockchain/encoding.html#public-key-cryptography
    private def encodePublicKey(key: Key): Array[Byte] = key.tpe match {
      case "tendermint/PubKeyEd25519" =>
        val prefixAndSizeArray: Array[Byte] = Array(0x16, 0x24, 0xDE, 0x64, 0x20).map(_.toByte)
        assert(key.value.size == 0x20)
        prefixAndSizeArray ++ key.value.toArray[Byte]
      case _ => throw new IllegalArgumentException("Unknown key type for" + key)
    }
  }
}
