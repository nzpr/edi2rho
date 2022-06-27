package coop.rchain.rabbit2rho

import cats.Show
import cats.effect.kernel.Resource
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, Concurrent, IO, IOApp}
import coop.rchain.rabbit2rho.conf.AppConf.{exchangeName, fs2RabbitConfig}
import dev.profunktor.fs2rabbit.config.Fs2RabbitConfig
import dev.profunktor.fs2rabbit.interpreter.RabbitClient
import dev.profunktor.fs2rabbit.model.ExchangeType.Topic
import dev.profunktor.fs2rabbit.model.{QueueName, RoutingKey}

import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.Random
import fs2.Stream
import cats.syntax.all._
import coop.rchain.rabbit2rho.conf.AppConf
import coop.rchain.rabbit2rho.transformers.smiths.Simple
import dev.profunktor.fs2rabbit.config.declaration._
import fs2.io.file.Path

object InputMock extends IOApp.Simple {

  /** RabbitMQ producer. */
  def producerStream[F[_]: Concurrent: Async, T: Show](
      dataQueues: List[QueueName],
      nextValue: QueueName => F[T],
      fs2RabbitConfig: Fs2RabbitConfig
  ): Stream[F, Unit] = {
    val resources = for {
      rabbit     <- RabbitClient.default(fs2RabbitConfig).resource
      connection <- rabbit.createConnectionChannel
      _          <- Resource.liftK(rabbit.declareExchange(exchangeName, Topic)(connection))
    } yield (rabbit, connection)

    Stream.resource(resources).flatMap {
      case (rabbit, connection) =>
        implicit val c = connection
        val producer = Stream
          .eval(dataQueues.traverse { q =>
            val rk = RoutingKey(q.value)
            val queueCfg =
              DeclarationQueueConfig(q, Durable, NonExclusive, NonAutoDelete, Map())
            rabbit.declareQueue(queueCfg) >>
              rabbit.bindQueue(q, exchangeName, rk) >>
              rabbit.createPublisher(exchangeName, rk).map(q -> _)
          })
          .flatMap { publishers =>
            val publishStream = publishers.zipWithIndex.map {
              case ((q, publishF), _) =>
                val rnd = new Random()
                Stream
                  .eval(
                    nextValue(q).flatMap(v => publishF(v.show).as(println(s"${q.value} -> ")))
                  )
                  .evalMap(_ => Async[F].sleep((rnd.nextInt(60) - 20).seconds))
                  .repeat
            }
            Stream.emits(publishStream).parJoinUnbounded
          }

        val receiver = {
          val outQueueStr = AppConf.appConf.rabbitConfig.rabbitOutputTopic
          val queueName   = QueueName(outQueueStr)
          val rk          = RoutingKey(queueName.value)
          val queueCfg =
            DeclarationQueueConfig(queueName, Durable, NonExclusive, NonAutoDelete, Map())
          Stream.eval(
            rabbit.declareQueue(queueCfg) >>
              rabbit.bindQueue(queueName, exchangeName, rk)
          )
        }
        producer concurrently receiver
    }
  }

  def inputMock(q: QueueName): IO[String] = {
    def rndInput: IO[String] = {
      val rndFile        = new java.io.File(AppConf.appConf.inputIdocPath).listFiles.toSet.head
      val bufferedSource = Source.fromFile(rndFile)
      val content        = bufferedSource.getLines().mkString("\n")
      bufferedSource.close()
      content.pure[IO]
    }

    q.value.split("-").last match {
      case "i1" => rndInput
      case "i2" => rndInput
    }
  }

  override def run: IO[Unit] = {
    val transformer = Simple[IO](Path(AppConf.appConf.mappingRulesPath)).unsafeRunSync()
    producerStream[IO, String](
      transformer.inputTags.map(QueueName).toList,
      inputMock,
      fs2RabbitConfig(AppConf.appConf)
    ).compile.drain
  }
}
