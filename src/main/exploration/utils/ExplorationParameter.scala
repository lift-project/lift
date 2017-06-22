package exploration.utils

import org.clapper.argot.{FlagOption, SingleValueOption}

/**
  * Created by bastian on 6/19/17.
  */
object ExplorationParameter {

  def getValue[T](option: SingleValueOption[T], config: Option[T], default: T) = {
    if (option.value.isDefined && config.isDefined && config.get != default && config.get != option.value.get)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option.value.getOrElse(config.getOrElse(default))
  }

  def getValue[T](option: FlagOption[T], config: Option[T], default: T) = {
    if (option.value.isDefined && config.isDefined && config.get != default && config.get != option.value.get)
      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")

    option.value.getOrElse(config.getOrElse(default))
  }
}
