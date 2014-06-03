package test



sealed abstract class Fun() {
  var context : Context = null;  

  var inT: Type = UndefType;
  var ouT: Type = UndefType;
  
  def setContext(ctx: Context): Fun = {
    if (ctx != null)
      this.context = ctx
    this
  }
  
  //override def toString() = {
  //  "["+super.toString
  //}

}

object Fun {
  
  def visit[T](z:T)(f: Fun, visitfn: (Fun,T) => T): T = {
    val result = visitfn(f,z)
    f match {
      case FPattern(inF, _) => visit(result)(inF, visitfn)
      case cf: CompFun => cf.funs.foldRight(result)((inF,x)=>visit(x)(inF, visitfn))      
      case _ => result
    }
  }
  
}


case object NullFun extends Fun {
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



abstract class Pattern() extends Fun()

object Pattern {
  
  def unapply(p: Pattern) : Option[Context] = Some(p.context)     
  
  /*def evalPerf(f: Fun): Float = {
    0f
  }*/
}


abstract class FPattern(f: Fun) extends Pattern() {
  def fun = f
}
object FPattern {
  def unapply(fp: FPattern): Option[(Fun,Context)] = Some(fp.fun,fp.context)
}

abstract class AbstractMap(f:Fun) extends FPattern(f)
object AbstractMap {
	def unapply(am: AbstractMap): Option[Fun] = Some(am.fun)
}
case class Map(f:Fun) extends AbstractMap(f) 
case class MapSeq(f: Fun) extends AbstractMap(f)
case class MapGlb(f: Fun) extends AbstractMap(f)
case class MapWrg(f: Fun) extends AbstractMap(f)
case class MapLcl(f: Fun) extends AbstractMap(f)

abstract class AbstractReduce(f:Fun) extends FPattern(f)
object AbstractReduce {
	def unapply(ar: AbstractReduce): Option[Fun] = Some(ar.fun)
}
case class Reduce(f: Fun) extends AbstractReduce(f)
case class ReduceSeq(f: Fun) extends AbstractReduce(f)

case class PartRed(f: Fun) extends FPattern(f)

case class oJoin() extends Pattern()
case class iJoin() extends Pattern()
case class oSplit(val chunkSize: Expr) extends Pattern()
case class iSplit(val chunkSize: Expr) extends Pattern()