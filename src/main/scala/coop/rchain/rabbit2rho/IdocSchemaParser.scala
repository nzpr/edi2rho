package coop.rchain.rabbit2rho

import cats.effect.kernel.Concurrent
import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.syntax.all._
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import coop.rchain.rabbit2rho.conf.AppConf
import fs2.io.file._
import fs2.{text, Stream}

object IdocSchemaParser extends IOApp {
  val json = JsonMapper
    .builder()
    .addModule(DefaultScalaModule)
    .build()

  final case class Element(descr: String, typ: String, offset: Int, size: Int)

  def readSchema[F[_]: Files: Concurrent](
      path: Path
  ): Stream[F, Map[String, Map[String, Element]]] =
    Stream.eval(Ref.of("")).covary[F].flatMap { recordRef =>
      Files[F]
        .readAll(path)
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(s => !s.isBlank)
        .map(_.trim)
        .dropWhile(s => !s.startsWith("INPUT Record Details"))
        .takeWhile(s => !s.startsWith("OUTPUT Record Details"))
        .evalMap {
          case s if s.startsWith("Record") =>
            val elPattern = "(?<=Tag).*?(?=at)".r
            val newRecord = elPattern.findFirstIn(s).get.trim
            recordRef.set(newRecord).as(none[String])
          case s if !s.contains("Left")    => none[String].pure
          case s if s.startsWith("filler") => none[String].pure
          case s                           => s.some.pure
        }
        .unNone
        .map(_.split("(  )+").map(_.trim))
        .map(vs => if (vs.length == 7) Array(vs.head, "") ++ vs.tail else vs)
        .map { vs =>
          val typ = {
            val t = vs(4).split(" ").last
            if (t == "Date/Time") vs(5) else t
          }
          vs.take(2) :+ typ :+ vs(6) :+ vs(7).split(" ").head
        }
        .evalScan(Map.empty[String, Map[String, Element]]) { (acc, vs) =>
          recordRef.get.map { curRecord =>
            val curV        = acc.getOrElse(curRecord, Map())
            val elementName = vs(0).split(":").head.filterNot(_ == '*')
            val newV        = curV + (elementName -> Element(vs(1), vs(2), vs(3).toInt, vs(4).toInt))
            acc + (curRecord -> newV)
          }
        }
    }

  def parseIdoc(
      idoc: String,
      schema: Map[String, Map[String, Element]]
  ): Stream[fs2.Pure, (String, Map[String, String])] =
    Stream(idoc)
      .through(text.lines)
      .filterNot(_.isBlank)
      .filterNot(_.contains("EDI_DC40"))
      .map {
        case s =>
          val elementOpt = schema.find { case (tag, _) => s.startsWith(tag) }
          assert(elementOpt.isDefined, s"Unknown element $s")
          val (tag, datasSchema) = elementOpt.get
          tag -> datasSchema.map {
            case (elemName, e) =>
              val valueStr =
                if (e.offset > s.length) ""
                else {
                  val end = Math.min(e.offset + e.size - 1, s.length)
                  s.substring(e.offset - 1, end).trim
                }
              val value = valueStr.nonEmpty.guard[Option].as(valueStr)
//                if (valueStr.isBlank) none[Any]
//                else
//                  Some(e.typ match {
//                    case "String"  => s"${valueStr}"
//                    case "Numeric" => valueStr.toFloat
//                    // date/time template otherwise
//                    case dateTimePattern => {
//                      val formatter = new SimpleDateFormat(dateTimePattern)
//                      formatter.parse(valueStr)
//                    }
//                  })
              (elemName, value)
          }
      }
      .map { case (k, v) => k -> v.collect { case (k, Some(v)) => (k, v) } }

  def idoc2RhoList(
      idoc: String,
      schema: Map[String, Map[String, Element]]
  ): List[String] =
    parseIdoc(idoc, schema).compile.toList
      .map { case (k, v) => Map(k -> v) }
      .map(json.writeValueAsString)

  def idoc2RhoMap(
      idoc: String,
      schema: Map[String, Map[String, Element]]
  ): Map[String, String] =
    parseIdoc(idoc, schema).compile.toList
      .map { case (el, vMap) => Map(el -> List(vMap)) }
      .combineAll
      .mapValues(json.writeValueAsString)

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val f = Files[IO]

    val idocsFolder = Path(AppConf.appConf.inputIdocPath)
    val mapRules    = Path(AppConf.appConf.mappingRulesPath)
    for {
      schema    <- readSchema[IO](mapRules).compile.lastOrError
      idocsPath = fs2.io.file.Files(f).list(idocsFolder)
      parsed <- idocsPath
                 .map { path =>
                   val source = scala.io.Source.fromFile(path.show)
                   try source.getLines.mkString("\n")
                   finally source.close()
                 }
                 .map(idoc2RhoList(_, schema))
                 .compile
                 .toList
      _ = println(parsed.head)
    } yield ExitCode.Success
  }
}
