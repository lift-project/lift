package cbackends.common.utils.pattern_matching



trait IsDefinedAt[T] {

    def apply(x: T) : T
    def isDefinedAt(x: T) = {
      try{
        apply(x)
        true
      } catch {
        case _: Exception =>
          false
      }

    }
}

