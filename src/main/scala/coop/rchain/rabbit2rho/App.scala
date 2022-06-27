package coop.rchain.rabbit2rho

import cats.Show
import cats.effect.kernel.Async
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
import coop.rchain.rabbit2rho.conf.AppConf
import coop.rchain.rabbit2rho.nodeclient.RNodeClient
import coop.rchain.rabbit2rho.transformers.smiths.Simple
import coop.rchain.rabbit2rho.transformers.{ContractDeployed, Transformer}
import fs2.Stream
import fs2.io.file.Path

object App extends IOApp.Simple {

  private def create[F[_]: Async, I: Show, R](
      tr: Transformer[I, R],
      sToI: String => I,
      rToS: R => String
  ) =
    Stream.resource[F, RNodeClient[F]](RNodeClient(AppConf.appConf.nodeConfig)).flatMap { rnode =>
      for {
        cDeployed    <- Stream.eval(ContractDeployed[F](rnode, tr.contract.name, tr.contract.term))
        totalResults <- Stream.eval(rnode.loadLatestInt(cDeployed.resultsTotal))
        _            <- Funnel[F, I, R](rnode, tr.contract, cDeployed.output, sToI, rToS, totalResults + 1)
      } yield ()
    }

  val transformer = Simple[IO](Path(AppConf.appConf.mappingRulesPath)).unsafeRunSync()

  def sToI(v: String): String = v
  def rToS(v: String): String = v
  implicit val s              = new Show[String] { override def show(t: String): String = t }

  implicit private val c     = Async[IO]
  override def run: IO[Unit] = create(transformer, sToI, rToS).compile.drain
}
