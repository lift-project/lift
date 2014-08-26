package ir
/*
object Printer {
  
  def toStringWithType(f: FunExpr) : String = {
    f match {
      case NullFunExpr =>  "null"
      case _:Pattern => "["+f.inT+"->"+f.ouT+"]"+f.toString()
      case cf: CompFunDef => cf.funs.map((f) => toStringWithType(f)).reduce((s1, s2) => s1 + " o " + s2)
    }
  }
  
}
*/