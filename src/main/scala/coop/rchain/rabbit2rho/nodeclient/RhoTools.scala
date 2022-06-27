package coop.rchain.rabbit2rho.nodeclient

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployDataProto
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GInt, GString}
import coop.rchain.models.GUnforgeable.UnfInstance.{GDeployerIdBody, GPrivateBody}
import coop.rchain.models.{ETuple, Expr, GDeployerId, GPrivate, GUnforgeable, Par}
import org.lightningj.util.ZBase32

import scala.annotation.tailrec

object RhoTools {
  def unforgeableNameRng(deployer: PublicKey, timestamp: Long): Blake2b512Random = {
    val seed =
      DeployDataProto().withDeployer(ByteString.copyFrom(deployer.bytes)).withTimestamp(timestamp)
    Blake2b512Random(DeployDataProto.toByteArray(seed))
  }

  def rng(signature: Array[Byte]): Blake2b512Random =
    Blake2b512Random(signature)

  def deployerIdWithTag(pk: PublicKey, tag: String): Par = {
    val outPar = Par(
      unforgeables = Seq(GUnforgeable(GDeployerIdBody(GDeployerId(ByteString.copyFrom(pk.bytes)))))
    )
    val idxPar = Par(exprs = Seq(Expr(GString(tag))))
    Par(exprs = Seq(Expr(ETupleBody(ETuple(ps = List(outPar, idxPar))))))
  }

  def unfNameWithIndex(name: GUnforgeable, idx: Int): Par = {
    val outPar = Par(unforgeables = Seq(name))
    val idxPar = Par(exprs = Seq(Expr(GInt(idx.toLong))))
    Par(exprs = Seq(Expr(ETupleBody(ETuple(ps = List(outPar, idxPar))))))
  }

  def singleExpr(p: Par): Option[Expr] =
    if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.matches.isEmpty && p.bundles.isEmpty) {
      p.exprs match {
        case Seq(single) => Some(single)
        case _           => None
      }
    } else (None)

  /** Get i-th unforgeable name from a deloy. */
  def unfName(deploy: Signed[DeployData], i: Int): GUnforgeable = {
    val seed             = unforgeableNameRng(deploy.pk, deploy.data.timestamp)
    val unfogreableBytes = Iterator.continually(seed.next()).drop(i).next()
    GUnforgeable(GPrivateBody(GPrivate(ByteString.copyFrom(unfogreableBytes))))
  }

  def uri(deploy: Signed[DeployData], i: Int): String = {
    def buildURI(arr: Array[Byte]): String = {
      val fullKey = new Array[Byte](34)
      Array.copy(arr, 0, fullKey, 0, 32)
      val crc = CRC14.compute(fullKey.view.slice(0, 32))
      fullKey(32) = (crc & 0xff).toByte
      fullKey(33) = ((crc & 0xff00) >>> 6).toByte
      "rho:id:" + ZBase32.encodeToString(fullKey, 270)
    }

    object CRC14 {
      val INIT_REMAINDER: Short = 0
      def update(rem: Short, b: Byte): Short = {
        @tailrec
        def loop(i: Int, rem: Short): Short =
          if (i < 8) {
            val shiftRem: Short = (rem << 1).toShort
            if ((shiftRem & 0x4000) != 0)
              loop(i + 1, (shiftRem ^ 0x4805).toShort)
            else
              loop(i + 1, shiftRem)
          } else {
            rem
          }
        loop(0, (rem ^ (b << 6).toShort).toShort)
      }

      def compute(b: IndexedSeq[Byte]) =
        b.foldLeft(INIT_REMAINDER)(update(_, _))
    }

    val seed         = unforgeableNameRng(deploy.pk, deploy.data.timestamp)
    val uriBytes     = Iterator.continually(seed.next()).drop(i).next()
    val hashKeyBytes = Blake2b256.hash(uriBytes)
    buildURI(hashKeyBytes)
  }
}
