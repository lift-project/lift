package ir



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

  def replaceRef(f: Fun, oriF: Fun, newF: Fun) : Fun = {
    visit(f,
      (inF: Fun) => if (inF.eq(oriF)) newF else inF,
      (inF: Fun) => inF)       
  }
  
  def visit[T](z:T)(f: Fun, vfn: (Fun,T) => T): T = {
    val result = vfn(f,z)
    f match {
      case FPattern(inF, _) => visit(result)(inF, vfn)
      case cf: CompFun => cf.funs.foldRight(result)((inF,x)=>visit(x)(inF, vfn))      
      case _ => result
    }
  }
  
  /*
   * Visit the expression of function f (recursively) and rebuild along the way
   */
  def visitExpr(f: Fun, exprF: (Expr) => (Expr)) : Fun = {   
    visit(f, inF => inF match {
      case oSplit(e) => oSplit(exprF(e))
      case asVector(e) => asVector(exprF(e))
      case _ => inF
    }, inF => inF)
  }  
  
  /*
   * Visit the function f recursively and rebuild along the way
   */
   def visit(f: Fun, pre: (Fun) => (Fun), post: (Fun) => (Fun)) : Fun = {
    var newF = pre(f)
    newF = newF match {
      case NullFun => f

      case cf: CompFun => CompFun(cf.funs.map(inF => visit(inF, pre, post)):_*)
      
      case Map(f)    => Map(visit(f,pre,post))
      case MapSeq(f) => MapSeq(visit(f,pre,post))
      case MapGlb(f) => MapGlb(visit(f,pre,post))
      case MapWrg(f) => MapWrg(visit(f,pre,post))
      case MapLcl(f) => MapLcl(visit(f,pre,post))
      
      case Reduce(f)    => Reduce(visit(f,pre,post))
      case ReduceSeq(f) => ReduceSeq(visit(f,pre,post))

      case PartRed(f) => PartRed(visit(f,pre,post))
      
      case _ => newF
    }
    post(newF)
  }

  /*
   * Visit the function f recursively
   */
  def visit(f: Fun, pre: (Fun) => (Unit), post: (Fun) => (Unit)): Unit = {
    pre(f)
    f match {
      case FPattern(inF, _) => visit(inF, pre, post)
      case cf: CompFun => cf.funs.reverseMap(inF => visit(inF, pre, post))
      case _ =>
    }
    post(f)
  }  
   
}

case object NullFun extends Fun {
    
  override def toString() = "null"
}

case class CompFun(val funs: Fun*) extends Fun {
    
  //def getFuns() = funs
  
  override def toString(): String = {
    /*"CompFun(" +*/ funs.map((f) => f.toString()).reduce((s1, s2) => s1 + " o " + s2) /*+ ")"*/
  }
  override def equals(o: Any) = {
    if (o.isInstanceOf[CompFun]) {
      var cf = o.asInstanceOf[CompFun];
      funs.seq.equals(cf.funs)
    } else
      false    
  }
  
  override def hashCode() = {
    funs.foldRight(3*79)((f,hash) => hash*f.hashCode())    
  }
}



abstract class Pattern() extends Fun() {
    def isGenerable() : Boolean
}

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
case class Map(f:Fun) extends AbstractMap(f)  {
  def isGenerable() = false
}

abstract class GenerableMap(f:Fun) extends  AbstractMap(f) {
    def isGenerable() = true
}

case class MapSeq(f: Fun) extends GenerableMap(f)
case class MapGlb(f: Fun) extends GenerableMap(f)
case class MapWrg(f: Fun) extends GenerableMap(f)
case class MapLcl(f: Fun) extends GenerableMap(f)

abstract class AbstractReduce(f:Fun) extends FPattern(f)
object AbstractReduce {
	def unapply(ar: AbstractReduce): Option[Fun] = Some(ar.fun)
}
case class Reduce(f: Fun) extends AbstractReduce(f) {
    def isGenerable() = false
}
case class ReduceSeq(f: Fun) extends AbstractReduce(f) {
      def isGenerable() = true
}

case class PartRed(f: Fun) extends FPattern(f) {
      def isGenerable() = false
}

case class oJoin() extends Pattern() {
   def isGenerable() = true
}
case class oSplit(val chunkSize: Expr) extends Pattern() {
   def isGenerable() = true
}

case class asScalar() extends Pattern() {
   def isGenerable() = true
}
case class asVector(val len: Expr) extends Pattern() {
   def isGenerable() = true
}