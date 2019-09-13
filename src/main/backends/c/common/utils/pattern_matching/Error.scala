package backends.c.common.utils.pattern_matching

object Error {

  def error[A](x: Option[A]) : Option[A] = {
    x match {
      case None => None
      case _ => assert(false, "pattern_not_match_error"); x
    }
  }

}
