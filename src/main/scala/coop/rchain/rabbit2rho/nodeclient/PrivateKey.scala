package coop.rchain.rabbit2rho.nodeclient

import com.google.protobuf.ByteString

import java.util.Arrays

final case class PrivateKey(bytes: Array[Byte]) {

  override def equals(o: Any): Boolean = o match {
    case other @ PrivateKey(_) => bytes.sameElements(other.bytes)
    case _                     => false
  }

  override def hashCode(): Int = Arrays.hashCode(bytes)
}

object PrivateKey {
  def apply(bs: ByteString): PrivateKey = new PrivateKey(bs.toByteArray)
}
