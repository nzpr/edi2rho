package coop.rchain.rabbit2rho.nodeclient

import casper.v1.{DeployResponse, DeployServiceFs2Grpc, RhoDataResponse}
import cats.effect.Async
import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all._
import coop.rchain.casper.protocol.{BlocksQuery, DataAtNameByBlockQuery}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.{Expr, GUnforgeable, Par}
import coop.rchain.rabbit2rho.conf.NodeConfig
import fs2.Stream
import io.grpc.Metadata
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder

import scala.concurrent.duration.DurationInt

final case class RNodeClient[F[_]: Sync](
    client: DeployServiceFs2Grpc[F, Metadata],
    nodeConfig: NodeConfig
) {
  private val pkBytes     = Base16.unsafeDecode(nodeConfig.deployerKey)
  private val deployerKey = PrivateKey(pkBytes)
  val pk                  = Secp256k1.toPublic(deployerKey)

  private def getLatestBlockNumber: F[Long] =
    client
      .getBlocks(BlocksQuery(1), new Metadata)
      .map(_.message.blockInfo.map(_.blockNumber))
      .head
      .collect { case Some(bn) => bn }
      .compile
      .lastOrError

  def prepareDeploy(term: String): F[Signed[DeployData]] =
    getLatestBlockNumber.map { vabn =>
      val timestamp = System.currentTimeMillis()
      val d = DeployData(
        term = term,
        timestamp = timestamp,
        phloPrice = nodeConfig.phloPrice.toLong,
        phloLimit = nodeConfig.phloLimit.toLong,
        validAfterBlockNumber = vabn,
        shardId = "root"
      )
      Signed(d, Secp256k1, deployerKey)
    }

  def deploy(term: String): F[DeployResponse] =
    prepareDeploy(term).flatMap { d =>
      client.doDeploy(DeployData.toProto(d), new Metadata)
    }

  def doDeploy(d: Signed[DeployData]): F[DeployResponse] =
    client.doDeploy(DeployData.toProto(d), new Metadata)

  def getDataOnName(par: Par, block: String): F[RhoDataResponse] = {
    val req = DataAtNameByBlockQuery(par, block)
    client.getDataAtName(req, new Metadata)
  }

  def getBlocks(qty: Int = 1): Stream[F, String] =
    client
      .getBlocks(BlocksQuery(qty), new Metadata)
      .map(_.message.blockInfo)
      .evalMap(_.liftTo[F](new Exception("wrong call")))
      .map(_.blockHash)

  def loadLatestInt(name: GUnforgeable): F[Long] =
    getBlocks()
      .evalMap(getDataOnName(Par(unforgeables = Seq(name)), _))
      .map(_.message.payload.map(_.par))
      .unNone
      .find(_.nonEmpty)
      .compile
      .last
      .map(
        _.map(v => RhoTools.singleExpr(v.head).collect { case Expr(GInt(v)) => v }.get).getOrElse(0)
      )
}

object RNodeClient {
  def apply[F[_]: Async](nodeConfig: NodeConfig): Resource[F, RNodeClient[F]] = {
    val builder = NettyChannelBuilder
      .forAddress(nodeConfig.host, nodeConfig.port)
      .negotiationType(io.grpc.netty.shaded.io.grpc.netty.NegotiationType.PLAINTEXT)

    def waitForNode(c: DeployServiceFs2Grpc[F, Metadata]): F[Unit] = {
      val pingStream = fs2.Stream
        .awakeEvery(1.second)
        .evalMap { _ =>
          c.getBlocks(BlocksQuery(1), ctx = new Metadata)
            .compile
            .lastOrError
            .map(_ => ().some)
            .handleError(_ => none[Unit])
        }

      pingStream
        .take(300) // 5 min to start the node
        .unNone
        .head
        .compile
        .last
        .map(_.liftTo(new Exception(s"Unable to establish connection to RNode")))
    }

    for {
      channel <- Resource.liftK(builder.build().pure[F])
      stub    <- DeployServiceFs2Grpc.stubResource(channel)
      _       = println(s"Waiting for RNode on localhost...")
      _       <- Resource.liftK(waitForNode(stub))
      _       = println(s"Connection to RNode established.")
    } yield RNodeClient(stub, nodeConfig)
  }

  def apply[F[_]: Async](implicit r: RNodeClient[F]): RNodeClient[F] = r
}
