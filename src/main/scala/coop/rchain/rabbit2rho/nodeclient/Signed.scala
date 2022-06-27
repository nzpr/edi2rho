package coop.rchain.rabbit2rho.nodeclient

import com.google.protobuf.ByteString

import java.nio.charset.StandardCharsets

final case class Signed[A] private (
  data: A,
  pk: PublicKey,
  sig: ByteString,
  sigAlgorithm: SignaturesAlg
) {
  // Protect Signed data to be copied (shadow generated copy function)
  // - hack is to prevent `unused error`
  // https://contributors.scala-lang.org/t/removing-copy-operation-from-case-classes-with-private-constructors/2605/2
  private def copy(): Unit = hack
  private def hack(): Unit = copy
}

object Signed {
  def apply[A: Serialize](
    data: A,
    sigAlgorithm: SignaturesAlg,
    sk: PrivateKey
  ): Signed[A] = {
    val serializedData = Serialize[A].encode(data).toArray
    val hash = signatureHash(sigAlgorithm.name, serializedData)
    val sig = sigAlgorithm.sign(hash, sk)

    Signed(data, sigAlgorithm.toPublic(sk), ByteString.copyFrom(sig), sigAlgorithm)
  }

  def fromSignedData[A: Serialize](
    data: A,
    pk: PublicKey,
    sig: ByteString,
    sigAlgorithm: SignaturesAlg
  ): Option[Signed[A]] = {
    val serializedData = Serialize[A].encode(data).toArray
    val hash = signatureHash(sigAlgorithm.name, serializedData)

    if (sigAlgorithm.verify(hash, sig.toByteArray, pk))
      Some(new Signed(data, pk, sig, sigAlgorithm))
    else
      None
  }

  def signatureHash(sigAlgName: String, serializedData: Array[Byte]) =
    sigAlgName match {
      case _ =>
        Blake2b256.hash(serializedData)
    }
}
