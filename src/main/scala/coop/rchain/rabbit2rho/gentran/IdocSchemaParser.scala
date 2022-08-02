package coop.rchain.rabbit2rho.gentran

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import coop.rchain.rabbit2rho.conf.AppConf
import coop.rchain.rabbit2rho.gentran.GentranTools._
import coop.rchain.rabbit2rho.gentran.IdocTools.idoc2RhoList
import fs2.io.file._

// Convert input IDOCs into format that is input for on chain contract
object ParseInput extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val f  = Files[IO]
    val idocsFolder = Path(AppConf.appConf.inputIdocPath)
    val mapRules    = Path(AppConf.appConf.mappingRulesPath)
    for {
      schema    <- readRecordsSchema[IO](mapRules)
      idocsPath = fs2.io.file.Files(f).list(idocsFolder)
      parsed <- idocsPath
                 .map { path =>
                   val source = scala.io.Source.fromFile(path.show)
                   try source.getLines.mkString("\n")
                   finally source.close()
                 }
                 .map(idoc2RhoList(_, schema._2, schema._1))
                 .compile
                 .toList
      _ = println(parsed.head)
    } yield ExitCode.Success
  }
}

// Initialize recursive map given the schema from Gentran mapping file.
object InitAMap extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val f = Files[IO]
    val mapRules   = Path(AppConf.appConf.mappingRulesPath)

    for {
      branching     <- readBranching[IO](mapRules)
      recordsSchema <- readRecordsSchema[IO](mapRules)
      map           = AMap[String, String](branching, recordsSchema._2.mapValues(_.keySet), "")
      _ = println(
        s"This is internal representation of an Amap (just in case it is helpful)\n$map\n"
      )
      _ = println("Try to modify root map, set BELNR to 123. Observe 123 is in BELNR now.")
      _ = map.set(("BELNR", "123"), "CLAIM3N".some)
      _ = println(map)
      _ = println("Get BELNR value directly on root map. Observe it is 123.")
      _ = println(map.get("BELNR"))
    } yield ExitCode.Success
  }
}

// Create contract for processing a single job
object AMapContract extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val f = Files[IO]
    val mapRules   = Path(AppConf.appConf.mappingRulesPath)
    for {
      r <- aMap(mapRules)
      _ = println(r)
    } yield ExitCode.Success
  }
}

// Create contract for processing a single job
object MkRecordContract extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val f = Files[IO]
    val mapRules   = Path(AppConf.appConf.mappingRulesPath)

    for {
      r <- extendedRules(mapRules)
      _ = println((r._1))
      _ = println((r._2))
      _ = println((r._3))
    } yield ExitCode.Success
  }
}

// Create contract for processing a single job
object MkFullContract extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val f = Files[IO]
    val mapRules   = Path(AppConf.appConf.mappingRulesPath)

    for {
      r <- contract(mapRules)
      _ = println(r)
    } yield ExitCode.Success
  }
}
