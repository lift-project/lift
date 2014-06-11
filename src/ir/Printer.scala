package ir

object Printer {
  
  def toStringWithType(f: Fun) : String = {    
    f match {
      case NullFun =>  "null"
      case _:Pattern => "["+f.inT+"->"+f.ouT+"]"+f.toString()
      case cf: CompFun => cf.funs.map((f) => toStringWithType(f)).reduce((s1, s2) => s1 + " o " + s2)
    }
  }
  
}