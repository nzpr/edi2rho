package coop.rchain.rabbit2rho.gentran

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.syntax.all._
import coop.rchain.rabbit2rho.gentran.IdocTools.Element
import fs2.io.file.{Files, Path}
import fs2.{text, Stream}

object GentranTools {

  /**
    * Read branching from Gentran mapping rules file.
    * Outputs a list of branches to feed [[AMap.init()]]
    */
  def readBranching[F[_]: Files: Concurrent](
      path: Path
  ): F[List[(Option[String], String)]] = {
    def leadingSpaces(s: String): Int = {
      val elPattern = " *?(?=\\S)".r
      val spaces    = elPattern.findFirstIn(s).get
      spaces.length
    }

    Stream
      .eval(Ref.of(Vector.empty[String]))
      .covary[F]
      .flatMap { prefixRef =>
        Files[F]
          .readAll(path)
          .through(text.utf8.decode)
          .through(text.lines)
          .filter(s => !s.isBlank)
          .dropWhile(s => !s.contains("INPUT Branching Diagram"))
          .takeWhile(s => !s.contains("OUTPUT Branching Diagram"))
          .drop(3)
          .map(_.split("\\*")(0).replaceAll("Record ", "").replaceAll("Group ", ""))
          .zipWithPrevious
          .evalTap {
            case (prevOpt, c) =>
              prefixRef.update { cur =>
                val shiftTo = prevOpt.filter(leadingSpaces(_) < leadingSpaces(c)).map(_.trim)
                val back    = prevOpt.exists(leadingSpaces(_) > leadingSpaces(c))
                if (back) cur.dropRight(1) else shiftTo.map(cur :+ _) getOrElse (cur)
              }
          }
          .map(_._2.trim)
          .evalMap { k =>
            val curRoot = prefixRef.get.map(_.lastOption)
            curRoot.map(_ -> k)
          }
      }
      .compile
      .toList
  }

  /** Read schemas for records from Gentran mapping rules file.
    * Returns tuple of
    *   1. Map from record ID (used in extended rules) to record Tag (used in input documents).
    *   2. Map record ID -> schema for reading values
    */
  def readRecordsSchema[F[_]: Files: Concurrent](
      path: Path
  ): F[(Map[String, String], Map[String, Map[String, Element]])] =
    Stream
      .eval(Ref.of("").flatMap(r => Ref.of(Map.empty[String, String]).map((_, r))))
      .covary[F]
      .flatMap {
        case (tagsMapRef, recordRef) =>
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
                val elPattern    = "(?<=Record ).*?(?=\\*)".r
                val tagPattern   = "(?<=Tag).*?(?=at)".r
                val newRecord    = elPattern.findFirstIn(s).get.trim
                val newRecordTag = tagPattern.findFirstIn(s).get.trim
                (recordRef.set(newRecord) >>
                  tagsMapRef.update(_ + (newRecord -> newRecordTag))).as(none[String])
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
                val curV = acc.getOrElse(curRecord, Map())
                val elementName =
                  vs(0).split("\\*").head.filterNot(_ == '*').replace(":", "_colon_")
                val newV = curV + (elementName -> Element(vs(1), vs(2), vs(3).toInt, vs(4).toInt))
                acc + (curRecord -> newV)
              }
            }
            .evalMap(recordsMap => tagsMapRef.get.map(v => (v, recordsMap)))
      }
      .compile
      .lastOrError

  def processJobContract[F[_]: Files: Concurrent](mappingRulesPath: Path): F[String] =
    for {
      branching     <- readBranching[F](mappingRulesPath)
      recordsSchema <- readRecordsSchema[F](mappingRulesPath)
    } yield {
      val localBranch = (none[String], "LOCAL")
      val branchesRho =
        s"[${(localBranch +: branching)
          .map { case (kOpt, k) => s"""("${kOpt.getOrElse("")}", "$k")""" }
          .mkString(",")}]"
      val recordsMapRho = s"""{${recordsSchema._2
        .mapValues(_.keySet)
        .map { case (k, v) => s""""$k": [${v.map(x => s""""$x"""").toList.mkString(", ")}]""" }
        .mkString(", ")}}"""

      s"""
       |new claim3n in {
       |  // return channel, map {"LOCAL": <list of keys in local map>}, contract to process a single record
       |  contract claim3n(ret, localsMap, processRecord) = {
       |    let 
       |      // these two values are parsed from gentran mapping rules file from branching and input records section
       |      branching <- $branchesRho;
       |      records <- $recordsMapRho;
       |      initState <- {} // TODO make AMap available in rholang AMap(branching, records.union(localsMap))
       |    in {
       |      new this, stateCh in {
       |        ret!(*this) // spit out the contract
       |      | stateCh!(*initState) // init state
       |      | contract this(ret, @"getResult") = { 
       |          for (state <<- stateCh) { ret!(*state) }
       |        }
       |      | contract this(record, @"processRecord") = { 
       |          for (state <- stateCh) {
       |            for (newState <- processRecord!?(*state, *record)) {
       |              stateCh!(*newState)
       |            }
       |          }
       |        }
       |      }   
       |    } 
       |  }  
       |}
       | """.stripMargin
    }
}
