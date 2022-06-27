package coop.rchain.rabbit2rho.nodeclient

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployDataProto
import scodec.bits.ByteVector

final case class DeployData(
    term: String,
    timestamp: Long,
    phloPrice: Long,
    phloLimit: Long,
    validAfterBlockNumber: Long,
    shardId: String
) {
  def totalPhloCharge = phloLimit * phloPrice
}

object DeployData {
  implicit val serialize = new Serialize[DeployData] {
    override def encode(a: DeployData): ByteVector =
      ByteVector(toProto(a).toByteArray)

    override def decode(bytes: ByteVector): Either[Throwable, DeployData] =
      Right(fromProto(DeployDataProto.parseFrom(bytes.toArray)))
  }

  private def fromProto(proto: DeployDataProto): DeployData =
    DeployData(
      proto.term,
      proto.timestamp,
      proto.phloPrice,
      proto.phloLimit,
      proto.validAfterBlockNumber,
      proto.shardId
    )

  def from(dd: DeployDataProto): Either[String, Signed[DeployData]] =
    for {
      algorithm <- SignaturesAlg(dd.sigAlgorithm).toRight("Invalid signing algorithm")
      signed <- Signed
                 .fromSignedData(fromProto(dd), PublicKey(dd.deployer), dd.sig, algorithm)
                 .toRight("Invalid signature")
    } yield signed

  private def toProto(dd: DeployData): DeployDataProto =
    DeployDataProto()
      .withTerm(dd.term)
      .withTimestamp(dd.timestamp)
      .withPhloPrice(dd.phloPrice)
      .withPhloLimit(dd.phloLimit)
      .withValidAfterBlockNumber(dd.validAfterBlockNumber)
      .withShardId(dd.shardId)

  def toProto(dd: Signed[DeployData]): DeployDataProto =
    toProto(dd.data)
      .withDeployer(ByteString.copyFrom(dd.pk.bytes))
      .withSig(dd.sig)
      .withSigAlgorithm(dd.sigAlgorithm.name)
}
