package test

abstract class Fun(ctx : Context) {
  var context : Context = ctx;
  //if (ctx != null) updateContext(ctx)


  /*
   * Update the context recursively
   */
  def updateContext(ctx: Context): Fun = {
    if (ctx != null) {
      context = ctx
      this match {
        case AbstractMap(f, _) => f.updateContext(ctx.incMapDepth)
        case FPattern(f, _) => f.updateContext(ctx.copy)
        case cf: CompFun => cf.funs.map(inF => inF.updateContext(ctx.copy))
      }
    }
    return this
  }
  
  
}

class NullFun extends Fun(new Context())

case class CompFun(funs: Seq[Fun], ctx: Context) extends Fun(ctx) {
  def this(fs: Fun*) = this(fs, null)
  
  def getFuns() = funs
  
  override def toString(): String = {
    /*"CompFun(" +*/ funs.map((f) => f.toString()).reduce((s1, s2) => s1 + " o " + s2) /*+ ")"*/
  }
  override def equals(o: Any) = {
    if (o.isInstanceOf[CompFun]) {
      var cf = o.asInstanceOf[CompFun];
      funs.seq.equals(cf.getFuns)
    } else
      false
  }
  
  override def hashCode() = {
    funs.hashCode()
  }
}
