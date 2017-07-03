package exploration.utils

import org.clapper.argot.{FlagOption, SingleValueOption}

object ExplorationParameter {

  def getValue[T](option: SingleValueOption[T], config: Option[T], default: T): T =
    getValue(option.value, config, default)

  def getValue[T](option: FlagOption[T], config: Option[T], default: T): T =
    getValue(option.value, config, default)

  def getValue[T](option: Option[T], config: Option[T], default: T): T = {
    if (option.isDefined && config.isDefined && config.get != option.get)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option.getOrElse(config.getOrElse(default))
  }
}
