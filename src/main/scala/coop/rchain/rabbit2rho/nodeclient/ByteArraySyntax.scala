package coop.rchain.rabbit2rho.nodeclient

import com.google.protobuf.ByteString

trait ByteArraySyntax {
  implicit final def modelsSyntaxByteArray(bs: Array[Byte]): ByteArrayOps =
    new ByteArrayOps(bs)

}

class ByteArrayOps(private val ba: Array[Byte]) extends AnyVal {
  def toByteString: ByteString = ByteString.copyFrom(ba)
  def toHexString: String      = Base16.encode(ba)
}
