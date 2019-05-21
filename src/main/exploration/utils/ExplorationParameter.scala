package exploration.utils

import play.api.libs.json._

import scala.collection.immutable.ListMap

object ExplorationParameter {

  def getValue[T](option: Option[T], config: Option[T], default: T): T = {
    if (option.isDefined && config.isDefined && config.get != option.get)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option.getOrElse(config.getOrElse(default))
  }

  def getValue[T](option: T, config: Option[T], default: T): T = {
    if (config.isDefined && config.get != option)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option
  }

  def getValue[T](option: Option[T], config: T): T = {
    if (option.isDefined && config != option.get)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option.getOrElse(config)
  }

  def getValue[T](option: T, config: T): T = {
    if (config != option)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option
  }

  def checkUnwantedEntry[T](jsv: JsValue, validKeys: Set[String], result: JsSuccess[T]) = {
    val obj = jsv.asInstanceOf[JsObject]
      val keys = obj.keys
      val unwanted = keys.diff(validKeys)
      if (unwanted.isEmpty) {
        result
      } else {
        JsError(s"Keys: ${unwanted.mkString(",")} found in the incoming JSON")
      }
  }

  def reads[T](jsv: JsValue, read: Reads[T], keys: Set[String]) : JsResult[T] = {
      read.reads(jsv).flatMap { entry =>
        val obj = jsv.asInstanceOf[JsObject]

        ExplorationParameter.checkUnwantedEntry[T](
          obj, keys, JsSuccess(entry))
      }
    }

  def generateConfigString(key: String, parameters: ListMap[String, Any]): String = {
    s"""{
       |${generateInnerJson(key, parameters)}
       |}
     """.stripMargin
  }

  def generateInnerJson(key: String, parameters: ListMap[String, Any]): String = {
    s"""
       |  "$key" : {
       |${parameters.zipWithIndex.map { x =>
      val name = x._1._1
      val value = x._1._2
      val i = x._2
      s"""    "$name": ${
        value match {
          case s: String => s""""$s"""" // wrap string args in ""
          case _ => value.toString()
        }
      }${
        if (i < parameters.size - 1) // drop comma for last parameter
          ","
        else ""
      }"""
    }.mkString("\n")}
       |  }""".stripMargin
  }
}
