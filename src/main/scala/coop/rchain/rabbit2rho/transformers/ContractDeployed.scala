package coop.rchain.rabbit2rho.transformers

import cats.Show
import cats.effect.Sync
import cats.effect.kernel.Async
import cats.syntax.all._
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GInt}
import coop.rchain.models.GUnforgeable.UnfInstance.GPrivateBody
import coop.rchain.models._
import coop.rchain.rabbit2rho.nodeclient.{Base16, PublicKey, RNodeClient, RhoTools}
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt

/** Contract deployed is represented by number of input ports and and and output port. */
final case class ContractDeployed(
    inputs: Seq[GUnforgeable],
    output: GUnforgeable,
    resultsTotal: GUnforgeable,
    inputTags: GUnforgeable
)

object ContractDeployed {

  implicit val s = new Show[ContractDeployed] {
    override def show(t: ContractDeployed): String =
      s"""inputs:
         |${t.inputs
           .map(_.unfInstance.gPrivateBody.get.id.toByteArray)
           .map(Base16.encode)
           .mkString("\n")}
         |output: 
         |${Base16.encode(t.output.unfInstance.gPrivateBody.get.id.toByteArray)}
         |input tags: 
         |${Base16.encode(t.inputTags.unfInstance.gPrivateBody.get.id.toByteArray)}
         |results total: 
         |${Base16.encode(t.resultsTotal.unfInstance.gPrivateBody.get.id.toByteArray)}
         |""".stripMargin
  }

  private def decode(v: Seq[Par]): Option[ContractDeployed] =
    v.headOption
      .flatMap(_.exprs.collectFirst {
        case Expr(ETupleBody(ETuple(pars, _, _))) if pars.size > 1 =>
          val is = pars.head.exprs.collectFirst {
            case Expr(ETupleBody(ETuple(pars, _, _))) =>
              pars
                .map(_.unforgeables.head.unfInstance.gPrivateBody.get.id)
                .map(id => GUnforgeable(GPrivateBody(GPrivate(id))))
          }.get
          val o = GUnforgeable(
            GPrivateBody(GPrivate(pars(1).unforgeables.head.unfInstance.gPrivateBody.get.id))
          )
          val tags = GUnforgeable(
            GPrivateBody(GPrivate(pars(2).unforgeables.head.unfInstance.gPrivateBody.get.id))
          )
          val rt = GUnforgeable(
            GPrivateBody(GPrivate(pars(3).unforgeables.head.unfInstance.gPrivateBody.get.id))
          )
          ContractDeployed(
            inputs = is,
            output = o,
            inputTags = tags,
            resultsTotal = rt
          )
      })

  private def checkResultName[F[_]: Sync](
      pk: PublicKey,
      contractName: String,
      rnode: RNodeClient[F]
  ): F[Option[Seq[Par]]] = {
    val contractPortsName = RhoTools.deployerIdWithTag(pk, contractName)
    rnode
      .getBlocks()
      .evalMap(rnode.getDataOnName(contractPortsName, _))
      .map(_.message.payload.map(_.par))
      .unNone
      .find(_.nonEmpty)
      .compile
      .last
  }

  private def waitForContract[F[_]: Async](
      rnode: RNodeClient[F],
      contractName: String,
      contractTerm: String
  ): F[Option[Seq[Par]]] =
    Stream
      .eval(SignallingRef.of(false))
      .flatMap { shouldDeployRef =>
        val deployOnce = shouldDeployRef.discrete.collectFirst { case true => () }.evalMap { _ =>
          println(s"Deploying contract $contractName.\n$contractTerm").pure[F] >>
            rnode.deploy(contractTerm).flatTap(r => println(r).pure[F])
        }
        val check = Stream
          .awakeEvery(1.second)
          .evalMap(_ => checkResultName[F](rnode.pk, contractName, rnode))
          .evalTap(r => shouldDeployRef.set(true).unlessA(r.nonEmpty))
          // wait for 100 seconds or declare failure
          .take(30)
          .unNone
          .head
        check concurrently deployOnce
      }
      .compile
      .last

  def apply[F[_]: Async](
      rnode: RNodeClient[F],
      contractName: String,
      contractTerm: String
  ): F[ContractDeployed] = {
    val errStr = s"Unable to deploy the contract. ${contractName}"
    waitForContract(rnode, contractName, contractTerm)
      .map(_.map(decode))
      .flatMap(_.flatten.liftTo(new Exception(errStr)))
      .flatTap(c => println(s"\nContract ${contractName} ready. \n${c.show}").pure[F])
  }
}
