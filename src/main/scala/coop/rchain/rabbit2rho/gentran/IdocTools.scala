package coop.rchain.rabbit2rho.gentran

import cats.syntax.all._
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import fs2.{text, Stream}

object IdocTools {
  val json = JsonMapper
    .builder()
    .addModule(DefaultScalaModule)
    .build()

  /** Key from record. */
  final case class Element(descr: String, typ: String, offset: Int, size: Int)

  def parseIdoc(
      idoc: String,
      schema: Map[String, Map[String, Element]],
      id2Tag: String => String
  ): Stream[fs2.Pure, (String, Map[String, String])] =
    Stream(idoc)
      .through(text.lines)
      .filterNot(_.isBlank)
      .map {
        case s =>
          val elementOpt = schema.find { case (id, _) => s.startsWith(id2Tag(id)) }
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
      schema: Map[String, Map[String, Element]],
      id2Tag: String => String
  ): List[String] =
    parseIdoc(idoc, schema, id2Tag).compile.toList
      .map { case (k, v) => Map(k -> v) }
      .map(json.writeValueAsString)

  def idoc2RhoMap(
      idoc: String,
      schema: Map[String, Map[String, Element]],
      id2Tag: String => String
  ): Map[String, String] =
    parseIdoc(idoc, schema, id2Tag).compile.toList
      .map { case (el, vMap) => Map(el -> List(vMap)) }
      .combineAll
      .mapValues(json.writeValueAsString)
}
