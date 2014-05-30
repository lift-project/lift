package test

abstract class Fun() {
  var context : Context = null;  

  /*
   * Update the context recursively
   */
  def updateContext(ctx: Context): Fun = {
    if (ctx != null) {
      this.context = ctx;
      this match {
        case AbstractMap(f, _) => f.updateContext(ctx.incMapDepth)
        case FPattern(f, _) => f.updateContext(ctx.copy)
        case cf: CompFun => cf.funs.map(inF => inF.updateContext(ctx.copy))
        case _ => 
      }
    }
    this
  }
  
  def setContext(ctx: Context): Fun = {
    if (ctx != null)
      this.context = ctx
    this
  }
}


object NullFun extends Fun {
  override def toString() = "null"
}

case class CompFun(funs: Fun*) extends Fun {  
  
  //def getFuns() = funs
  
  override def toString(): String = {
    /*"CompFun(" +*/ funs.map((f) => f.toString()).reduce((s1, s2) => s1 + " o " + s2) /*+ ")"*/
  }
  override def equals(o: Any) = {
    if (o.isInstanceOf[CompFun]) {
      var cf = o.asInstanceOf[CompFun];
      funs.seq.equals(cf.funs )
    } else
      false
  }
  
  override def hashCode() = {
    funs.hashCode()
  }
}
