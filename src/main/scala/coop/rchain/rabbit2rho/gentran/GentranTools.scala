package coop.rchain.rabbit2rho.gentran

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.syntax.all._
import coop.rchain.gentran2rho.Compiler.createRhoFromAST
import coop.rchain.gentran2rho.ERParser.sourceToAST
import coop.rchain.rabbit2rho.gentran.IdocTools.Element
import fs2.io.file.{Files, Path}
import fs2.{text, Stream}

import java.io.StringReader

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
                  vs(0).split("\\*").head.filterNot(_ == '*')
                val newV = curV + (elementName -> Element(vs(1), vs(2), vs(3).toInt, vs(4).toInt))
                acc + (curRecord -> newV)
              }
            }
            .evalMap(recordsMap => tagsMapRef.get.map(v => (v, recordsMap)))
      }
      .compile
      .lastOrError

  def aMap[F[_]: Files: Concurrent](
      mappingRulesPath: Path
  ): F[String] = {
    def aMapDeclaration(
        branching: List[(Option[String], String)],
        recordsSchema: (Map[String, String], Map[String, Map[String, Element]])
    ): String = {

      def getChannels(chanName: String, idxs: List[Int]): List[String] =
        idxs.map(idx => s"${chanName}_$idx")

      val helper                                             = AMapHelper(branching, recordsSchema._2.mapValues(_.keySet))
      val leafData: Map[String, List[Int]]                   = helper.getLeafData
      val branchData: List[(String, Map[String, List[Int]])] = helper.getBranchData
      val channels = if (leafData.nonEmpty) leafData.toList.flatMap {
        case (name, idxs) => getChannels(name, idxs)
      } else List("temp")
      val strNew = channels.mkString(", ")
      val strState =
        if (leafData.nonEmpty)
          leafData.toList
            .map {
              case (name, lData) =>
                val lChans = getChannels(name, lData).map("*" + _)
                s""""$name":[${lChans.mkString(", ")}]"""
            }
            .mkString(", ")
        else ""
      val strBranches =
        if (branchData.nonEmpty)
          branchData
            .map {
              case (name, lData) =>
                val fields = lData.toList.map {
                  case (fName, fIdxs) =>
                    s""""$fName": [${fIdxs.mkString(", ")}]"""
                }
                s""""$name": {${fields.mkString(", ")}}"""
            }
            .mkString(",\n        ")
        else ""
      val strSendInit = channels.map(_ + "!(Nil)").mkString(" | ")
      val strWait     = channels.map(chan => s"; _ <<- $chan").mkString("")
      s"""  contract AMap(return, @"decl") = {
        #    new $strNew in {
        #      state!({$strState}) |
        #      branches!({
        #        $strBranches
        #      }) |
        #      $strSendInit |
        #      for(_ <<- state; _ <<- branches$strWait) {
        #        return!(true)
        #      }
        #    }
        #  }""".stripMargin('#')
    }
    for {
      branching     <- readBranching[F](mappingRulesPath)
      recordsSchema <- readRecordsSchema[F](mappingRulesPath)
      strDecl       = aMapDeclaration(branching, recordsSchema)
    } yield {
      s"""new AMap, state, branches in {
         #$strDecl |
         #  contract AMap(return, @"get", @prefix, @field) = {
         #    for(@s <<- state; @b <<- branches) {
         #      let @fieldIdxs <- b.get(prefix).get(field);
         #        @firstIdx <- fieldIdxs.nth(0);
         #        @fields <- s.get(field);
         #        chan <- fields.nth(firstIdx) in {
         #         for(@res <<- chan) {return!(res)}
         #      }
         #    }
         #  } |
         #  contract AMap(return, @"update", @prefix, @field, @value) = {
         #    for(@s <<- state; @b <<- branches) {
         #      let @fieldIdxs <- b.get(prefix).get(field);
         #        @fields <- s.get(field) in {
         #        new loop in {
         #          contract loop(@idxs) = {
         #            match idxs {
         #              [head ...tail] => {
         #                let chan <- fields.nth(head) in {
         #                  for(_ <- chan) {
         #                    chan!(value) | 
         #                    for( _ <<- chan ) {loop!(tail)}
         #                  }
         #                }
         #              }
         #              _ => { return!(true) }
         #            }
         #          } |
         #          loop!(fieldIdxs)
         #        } 
         #      }
         #    }
         #  }
         #}""".stripMargin('#')
    }
  }

  def extendedRules[F[_]: Files: Concurrent](
      mappingRulesPath: Path
  ): F[(String, String, String)] =
    Files[F]
      .readAll(mappingRulesPath)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(s => !s.isBlank)
      .dropWhile(s => !s.contains("Extended Rules"))
      .dropThrough(s => !s.contains("On Begin"))
      .takeWhile(s => !s.contains("CLADET"))
      .compile
      .toList
      .map(_.mkString("\n"))
      .map { rules =>
        val reader = new StringReader(rules ++ "\n")
        val ast    = sourceToAST(reader)
        createRhoFromAST(ast)
      }

  def contract[F[_]: Files: Concurrent](
      mappingRulesPath: Path
  ): F[String] =
    for {
      er                                   <- extendedRules(mappingRulesPath)
      (localMap, localInit, processRecord) = er
      amap                                 <- aMap(mappingRulesPath)
    } yield s"""
         #new claim3n, processRecord in {
         #$localMap |
         #$localInit |
         #$amap |
         #| $processRecord
         #}""".stripMargin('#')
}
