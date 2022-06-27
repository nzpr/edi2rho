package coop.rchain.rabbit2rho

import cats.effect.kernel.Async
import cats.effect.{Ref, Resource, Temporal}
import cats.syntax.all._
import coop.rchain.models.{GUnforgeable, Par}
import coop.rchain.rabbit2rho.conf.AppConf.{appConf, exchangeName, fs2RabbitConfig}
import coop.rchain.rabbit2rho.nodeclient.{RNodeClient, RhoTools}
import coop.rchain.rabbit2rho.transformers.Contract
import dev.profunktor.fs2rabbit.config.declaration._
import dev.profunktor.fs2rabbit.interpreter.RabbitClient
import dev.profunktor.fs2rabbit.model.AckResult.Ack
import dev.profunktor.fs2rabbit.model.ExchangeType.Topic
import dev.profunktor.fs2rabbit.model.{AmqpEnvelope, QueueName, RoutingKey}
import fs2.Stream

import scala.concurrent.duration.DurationInt

object Funnel {
  private def create[F[_]: Temporal, I, R](
      rabbitI: Stream[F, AmqpEnvelope[I]],
      rabbitO: R => F[Unit],
      deploy: String => F[Unit],
      loadResult: (GUnforgeable, Long) => F[Option[Seq[Par]]],
      contract: Contract[I, R],
      resultName: GUnforgeable,
      harvestFrom: Long
  ): Stream[F, Unit] = {
    def decodeInput(i: AmqpEnvelope[I]): Option[String] =
      contract.codec.input.get(i.routingKey.value).map(mkTerm => mkTerm(i.payload))

    def resultsSince(start: Long): Stream[F, Seq[Par]] =
      Stream.unfoldEval[F, Long, Seq[Par]](start) { i =>
        loadResult(resultName, i).map(rOpt => rOpt.filter(_.nonEmpty).map(_ -> (i + 1)))
      }

    val hydrationStream = rabbitI.map(decodeInput).unNone.evalMap(deploy)
    val harvestStream = Stream
      .eval(Ref.of[F, Long](harvestFrom))
      .flatMap { latestCollectedRef =>
        Stream
          .awakeEvery(1.second)
          .evalMap { _ =>
            latestCollectedRef.get
              .map(resultsSince(_).evalTap(_ => latestCollectedRef.update(v => v + 1)))
          }
      }
      .flatten
      .evalTap(r => contract.codec.result(r).traverse(rabbitO))

    hydrationStream concurrently harvestStream
  }

  def apply[F[_]: Temporal: Async, I, R](
      rnode: RNodeClient[F],
      contract: Contract[I, R],
      resultUnf: GUnforgeable,
      sToI: String => I,
      rToS: R => String,
      harvestFrom: Long
  ): Stream[F, Unit] = {
    val resources = for {
      rabbit     <- RabbitClient.default(fs2RabbitConfig).resource
      connection <- rabbit.createConnectionChannel
      _          <- Resource.liftK(rabbit.declareExchange(exchangeName, Topic)(connection))
      initRabbit = {
        implicit val c = connection
        val initConsumersF = for {
          v <- contract.codec.input.keySet.toList.traverse { sourceTag =>
                val queueName = QueueName(sourceTag)
                val rk        = RoutingKey(sourceTag)
                val queueCfg =
                  DeclarationQueueConfig(queueName, Durable, NonExclusive, NonAutoDelete, Map())
                rabbit.declareQueue(queueCfg) >>
                  rabbit.bindQueue(queueName, exchangeName, rk) >>
                  rabbit.createAckerConsumer(queueName)
              }
          toAckRef <- Ref.of(Set.empty[F[Unit]])
          r        = v.map { case (acker, stream) => stream.map(v => v -> acker) }
          streamWithAcks = Stream.emits(r).parJoinUnbounded.evalTap {
            case (i, ack) => toAckRef.update(v => v + ack(Ack(i.deliveryTag)))
          }
        } yield (toAckRef, streamWithAcks.map { case (i, _) => i })

        val initProducerF = {
          val outQueueStr = appConf.rabbitConfig.rabbitOutputTopic
          val rk          = RoutingKey(outQueueStr)
          val queueName   = QueueName(outQueueStr)
          val queueCfg =
            DeclarationQueueConfig(queueName, Durable, NonExclusive, NonAutoDelete, Map())
          rabbit.declareQueue(queueCfg) >>
            rabbit.bindQueue(queueName, exchangeName, rk) >>
            rabbit.createPublisher(exchangeName, rk).map(v => v)
        }

        initConsumersF.flatMap(in => initProducerF.map(in -> _))
      }
      rabbitPorts <- Resource.liftK(initRabbit)
    } yield {
      println(s"Rabbit ports initialized.\n")
      val ((toAckRef, rabbitIn), rabbitOut) = rabbitPorts
      def deploy(s: String) =
        rnode.deploy(s).void
      def loadResults(resultName: GUnforgeable, idx: Long) =
        for {
          latestBlock <- rnode.getBlocks().compile.lastOrError
          par         = RhoTools.unfNameWithIndex(resultName, idx.toInt)
          data        <- rnode.getDataOnName(par, latestBlock)
          resultNames = data.message.payload.map(_.par)
        } yield resultNames
      def sendResultWithAck(r: R) =
        rabbitOut.compose[R](rToS)(r) >> toAckRef
          .getAndUpdate(v => if (v.size == contract.codec.input.size) Set() else v)
          .flatTap { v =>
            if (v.size == contract.codec.input.size) v.toList.traverse(x => x).void
            else ().pure[F]
          }
          .void
      (
        rabbitIn.map(v => v.copy(payload = sToI(v.payload))),
        sendResultWithAck _,
        deploy _,
        loadResults _,
        contract,
        resultUnf,
        harvestFrom
      )
    }

    Stream.resource(resources).flatMap((create[F, I, R] _).tupled)
  }
}
