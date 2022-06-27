package coop.rchain.rabbit2rho.transformers.smiths

import cats.effect.kernel.Concurrent
import cats.syntax.all._
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.{Expr, Par}
import coop.rchain.rabbit2rho.IdocSchemaParser.{readSchema, Element}
import coop.rchain.rabbit2rho.IdocSchemaParser
import coop.rchain.rabbit2rho.conf.AppConf
import coop.rchain.rabbit2rho.nodeclient.RhoTools.singleExpr
import coop.rchain.rabbit2rho.transformers.{Codec, Contract, Transformer}
import fs2.io.file.{Files, Path}

import scala.io.Source

object Simple {
  private val arity           = 2
  private val contractName    = "smiths-alpha"
  private val contractVersion = "0.0.1"
  private val contractId      = s"$contractName-v$contractVersion"
  private val inputNames      = (1 to arity).map(i => s"i$i").toList
  // these are tags for incoming data
  private val inputTags    = inputNames.map(i => s"${contractName}-$i")
  private val inputTagsStr = inputTags.map(v => "\\\"" + v + "\\\"").mkString(",")

  private def term: String =
    Source
      .fromFile(AppConf.appConf.mainContractPath)
      .getLines()
      .mkString("\n")
      .replace("$$CONTRACT_ID$$", contractId)
      .replace("$$INPUT_CHANNEL_TAGS$$", inputTagsStr)

  private def call(nameIdx: Int, data: String): String =
    Source
      .fromFile(AppConf.appConf.produceContractPath)
      .getLines()
      .mkString("\n")
      .replace("\"$$NAME_IDX$$\"", nameIdx.toString)
      .replace("\"$$DATA$$\"", data)
      .replace("$$CONTRACT_ID$$", contractId)

  private def contract(inputSchema: Map[String, Map[String, Element]]): Contract[String, String] = {
    val portMapping = (inputTags zip inputNames).toMap
    val codec = new Codec[String, String] {
      override def input: Map[String, String => String] = portMapping.mapValues {
        name => (v: String) =>
          val parsed  = IdocSchemaParser.idoc2RhoList(v, inputSchema)
          val data    = s"[${parsed.mkString(", ")}]"
          val nameIdx = inputNames.indexOf(name) + 1
          call(nameIdx, data)
      }
      override def result: Seq[Par] => Option[String] =
        (in: Seq[Par]) =>
          in.headOption.map(singleExpr(_).collect { case Expr(GString(v)) => v }.get)
    }
    Contract(codec, term, contractId)
  }

  def apply[F[_]: Concurrent: Files](mappingRulesPath: Path) =
    for {
      inputSchema <- readSchema[F](mappingRulesPath).compile.lastOrError
    } yield new Transformer[String, String] {
      override def contractName: String               = Simple.contractName
      override def contractVersion: String            = Simple.contractVersion
      override def contract: Contract[String, String] = Simple.contract(inputSchema)
      override def inputNames: Seq[String]            = Simple.inputNames
      override def inputTags: Seq[String]             = Simple.inputTags
    }
}
