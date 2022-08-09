package coop.rchain.rabbit2rho.transformers.smiths

import cats.effect.kernel.Ref.Make
import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all._
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.{Expr, Par}
import coop.rchain.rabbit2rho.conf.AppConf
import coop.rchain.rabbit2rho.gentran.GentranTools.readRecordsSchema
import coop.rchain.rabbit2rho.gentran.{GentranTools, IdocTools}
import coop.rchain.rabbit2rho.gentran.IdocTools.Element
import coop.rchain.rabbit2rho.nodeclient.RhoTools.singleExpr
import coop.rchain.rabbit2rho.transformers.{Codec, Contract, Transformer}
import fs2.io.file.{Files, Path}

import scala.io.{BufferedSource, Source}

object Simple {
  private val arity           = 1
  private val contractName    = "smiths-alpha"
  private val contractVersion = "0.0.8"
  private val contractId      = s"$contractName-v$contractVersion"
  private val inputNames      = (1 to arity).map(i => s"i$i").toList
  // these are tags for incoming data
  private val inputTags = inputNames.map(i => s"${contractName}-$i")

  private def fileResource[F[_]: Sync](path: String): Resource[F, BufferedSource] =
    Resource.make(Sync[F].delay(Source.fromFile(path))) { v =>
      Sync[F].delay(v.close())
    }

  private def mkContractTerm[F[_]: Sync](mappingRulesPath: Path): F[String] =
    fileResource(AppConf.appConf.mainContractPath).use { topContract =>
      // TODO replace noOpMapping with real mapping
      GentranTools
        .mkMappingContract(mappingRulesPath)
        .map(
          topContract
            .getLines()
            .mkString("\n")
            .replace("$$CONTRACT_ID$$", contractId)
            .replace("$$doMapping$$", _)
        )
    }

  private def mkCallTerm[F[_]: Sync]: F[String] =
    fileResource(AppConf.appConf.produceContractPath).use { callContract =>
      callContract
        .getLines()
        .mkString("\n")
        .replace("$$CONTRACT_ID$$", contractId)
        .pure
    }

  private def mkContract[F[_]: Sync](
      inputSchema: (Map[String, String], Map[String, Map[String, Element]]),
      contractTerm: String,
      callTerm: String
  ): Contract[String, String] = {
    val portMapping = (inputTags zip inputNames).toMap
    val codec = new Codec[String, String] {
      override def input: Map[String, String => String] = portMapping.mapValues {
        _ => (v: String) =>
          val parsed = IdocTools.idoc2RhoList(v, inputSchema._2, inputSchema._1)
          val data   = s"[${parsed.mkString(", ")}]"
          callTerm.replace("\"$$DATA$$\"", data)
      }

      override def result: Seq[Par] => Option[String] =
        (in: Seq[Par]) =>
          in.headOption.map(singleExpr(_).collect { case Expr(GString(v)) => v }.get)
    }
    Contract(codec, contractTerm, contractId)
  }

  def apply[F[_]: Sync: Make: Files](mappingRulesPath: Path) =
    for {
      inputSchema  <- readRecordsSchema[F](mappingRulesPath)
      contractTerm <- mkContractTerm(mappingRulesPath)
      callTerm     <- mkCallTerm
      c            = mkContract(inputSchema, contractTerm, callTerm)
    } yield new Transformer[String, String] {
      override def contractName: String               = Simple.contractName
      override def contractVersion: String            = Simple.contractVersion
      override def contract: Contract[String, String] = c
      override def inputNames: Seq[String]            = Simple.inputNames
      override def inputTags: Seq[String]             = Simple.inputTags
    }
}
