package exploration.utils

import org.clapper.argot.{FlagOption, SingleValueOption}

/**
  * Created by bastian on 6/19/17.
  */
object ExplorationParameter {

  def getValue[T](option: SingleValueOption[T], config: Option[T], default: T) =
    option.value.getOrElse(config.getOrElse(default))

  def getValue[T](option: FlagOption[T], config: Option[T], default: T) =
    option.value.getOrElse(config.getOrElse(default))
}
