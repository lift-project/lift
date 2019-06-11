package cbackends.common.utils.pattern_matching

import ir.TypeException


trait IsDefinedAt[T] {

    def apply(x: T) : T
    def isDefinedAt(x: T) = {
      try{
        apply(x)
        true
      } catch {
        case t: ir.TypeException =>
          throw t
        case _: scala.MatchError =>
          false
      }

    }
}

