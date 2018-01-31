package exploration.utils

import org.clapper.argot.{FlagOption, SingleValueOption}
import play.api.libs.json._

object ExplorationParameter {

  def getValue[T](option: SingleValueOption[T], config: Option[T], default: T): T =
    getValue(option.value, config, default)

  def getValue[T](option: SingleValueOption[T], config: T): T =
    getValue(option.value, config)

  def getValue[T](option: FlagOption[T], config: Option[T], default: T): T =
    getValue(option.value, config, default)

  def getValue[T](option: FlagOption[T], config: T): T =
    getValue(option.value, config)

  def getValue[T](option: Option[T], config: Option[T], default: T): T = {
    if (option.isDefined && config.isDefined && config.get != option.get)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option.getOrElse(config.getOrElse(default))
  }

  def getValue[T](option: Option[T], config: T): T = {
    if (option.isDefined && config != option.get)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option.getOrElse(config)
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

  def reads[T](jsv: JsValue, read: Reads[T], jsonKey: String, keys: Set[String]) : JsResult[T] = {
      read.reads(jsv).flatMap { entry =>
        val obj = jsv.asInstanceOf[JsObject]

        val json = obj.fieldSet.collectFirst{ case (name, js) if name == jsonKey => js}
        ExplorationParameter.checkUnwantedEntry[T](
          json.get, keys, JsSuccess(entry))
      }
    }
}
