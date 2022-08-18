package coop.rchain.rabbit2rho.conf

import cats.syntax.all.none
import dev.profunktor.fs2rabbit.config.Fs2RabbitConfig
import dev.profunktor.fs2rabbit.model.ExchangeName
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import cats.syntax.all._

import java.io.File
import scala.concurrent.duration.DurationInt
import scala.io.Source

case class AppConf(
    rabbitConfig: RabbitConfig,
    nodeConfig: NodeConfig,
    inputIdocPath: String,
    mappingRulesPath: String,
    mainContractPath: String,
    produceContractPath: String
)

// config for rabbit consumer
case class RabbitConfig(
    host: String,
    port: Int,
    rabbitOutputTopic: String,
    exchangeName: String
)

case class NodeConfig(
    host: String,
    port: Int,
    deployerKey: String,
    phloPrice: Int,
    phloLimit: Int
)

object AppConf {

  implicit lazy val appConf: AppConf = {
    val conf = ConfigSource.default.load[AppConf].leftMap(println).toOption.get

    val mr =
      if (conf.mappingRulesPath.isBlank)
        new File("src/main/resources/smiths/mapping-rules.txt").getAbsolutePath
      else conf.mappingRulesPath
    val inputs =
      if (conf.inputIdocPath.isBlank)
        new File("src/main/resources/smiths/input").getAbsolutePath
      else conf.inputIdocPath
    val mContract =
      if (conf.mainContractPath.isBlank)
        new File("src/main/resources/rho/top.rhox").getAbsolutePath
      else conf.mainContractPath
    val pContract =
      if (conf.produceContractPath.isBlank)
        new File("src/main/resources/rho/call.rho").getAbsolutePath
      else conf.produceContractPath
    conf.copy(
      mappingRulesPath = mr,
      inputIdocPath = inputs,
      mainContractPath = mContract,
      produceContractPath = pContract
    )
  }
  def fs2RabbitConfig(implicit r: AppConf): Fs2RabbitConfig = Fs2RabbitConfig(
    r.rabbitConfig.host,
    r.rabbitConfig.port,
    "/",
    5.seconds,
    false,
    none[String],
    none[String],
    true,
    false,
    none[Int],
    10.seconds,
    true
  )

  def exchangeName(implicit r: AppConf): ExchangeName = ExchangeName(r.rabbitConfig.exchangeName)
}
