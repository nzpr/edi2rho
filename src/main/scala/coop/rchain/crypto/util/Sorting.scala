package coop.rchain.crypto.util

import coop.rchain.rabbit2rho.nodeclient.PublicKey
import scala.math.Ordering._

object Sorting {

  implicit val byteArrayOrdering = Ordering.by((_: Array[Byte]).toIterable)

  implicit val publicKeyOrdering: Ordering[PublicKey] = Ordering.by[PublicKey, Array[Byte]](_.bytes)

}
