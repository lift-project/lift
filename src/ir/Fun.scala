package ir

import opencl.ir._

abstract class Fun () {
  var context : Context = null

  // type information
  var inT: Type = UndefType
  var ouT: Type = UndefType

  // memory information
  var inM: Memory = UnallocatedMemory
  var outM: Memory = UnallocatedMemory

  /*
   * Is this immediate function generable? (without looking inside)
   */
  def isGenerable() : Boolean

  def copy(): Fun
  
  def setContext(ctx: Context): Fun = {
    if (ctx != null)
      this.context = ctx
    this
  }
  
  def o(that: Fun) : CompFun = {
    val thisFuns = this match {
      case cf : CompFun => cf.funs
      case _ => Seq(this)
    }
    val thatFuns = that match {
      case cf : CompFun => cf.funs
      case _ => Seq(that)
    }  
    val allFuns = thisFuns ++ thatFuns
    new CompFun(allFuns:_*)    
  }

}

object Fun {

  def replaceRef(f: Fun, oriF: Fun, newF: Fun) : Fun = {

    //println("f: "+f+" oriF: "+oriF+" newF: "+newF)

    if (f.eq(oriF))
      return newF

    f match {
      case NullFun => NullFun
      case cf: CompFun => CompFun(cf.funs.map(inF => replaceRef(inF, oriF, newF)):_*)
      case fp: FPattern => fp.getClass().getConstructor(classOf[Fun]).newInstance(replaceRef(fp.f,oriF,newF))
      case _ => f//f.copy()
    }
  }
  
  def visit[T](z:T)(f: Fun, vfn: (Fun,T) => T): T = {
    val result = vfn(f,z)
    f match {
      case fp: FPattern => visit(result)(fp.f, vfn)
      case cf: CompFun => cf.funs.foldRight(result)((inF,x)=>visit(x)(inF, vfn))      
      case _ => result
    }
  }
  
  /*
   * Visit the expression of function f (recursively) and rebuild along the way
   */
  def visitExpr(f: Fun, exprF: (Expr) => (Expr)) : Fun = {   
    visit(f, (inF:Fun) => inF match {
      case Split(e) => Split(exprF(e))
      case asVector(e) => asVector(exprF(e))
      case _ => inF
    }, (inF:Fun) => inF)
  }  
  
  /*
   * Visit the function f recursively and rebuild along the way
   */
   def visit(f: Fun, pre: (Fun) => (Fun), post: (Fun) => (Fun)) : Fun = {
    var newF = pre(f)
    newF = newF match {
      case NullFun => NullFun

      case cf: CompFun => CompFun(cf.funs.map(inF => visit(inF, pre, post)):_*)
      
      case fp: FPattern => fp.getClass().getConstructor(classOf[Fun]).newInstance(visit(fp.f,pre,post)) 
      
      case _ => newF.copy() 
    }
    
    assert (newF == NullFun || !newF.eq(f),"newF= "+newF+" f= "+f)
    
    post(newF)
  }

  /*
   * Visit the function f recursively
   */
  def visit(f: Fun, pre: (Fun) => (Unit), post: (Fun) => (Unit)): Unit = {
    pre(f)
    f match {
      case fp: FPattern => visit(fp.f, pre, post)
      case cf: CompFun => cf.funs.reverseMap(inF => visit(inF, pre, post))
      case _ =>
    }
    post(f)
  }
   
}


case object NullFun extends Fun {
  override def isGenerable() = true

  override def toString() = "null"
  override def copy() = NullFun
}

case class CompFun(val funs: Fun*) extends Fun {

  override def isGenerable() = true

  override def toString(): String = {
    funs.map((f) => f.toString()).reduce((s1, s2) => s1 + " o " + s2)
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
  
  override def copy() = new CompFun(funs.map(f => f.copy()):_*)

  // flatten all the composed functions
  def flatten : List[Fun] = {
    this.funs.foldLeft(List[Fun]())((l, f) => {
      f match {
        case cf: CompFun => l ++ cf.flatten
        case _ => l :+ f
      }
    })
  }
}

// Here are just the algorithmic patterns
// For opencl specific patterns see the opencl.ir package

abstract class Pattern() extends Fun()
object Pattern {
  
  def unapply(p: Pattern) : Option[Context] = Some(p.context)     

}

trait FPattern extends Pattern {
  def f: Fun
  def copy() : Fun = this.getClass().getConstructor(classOf[Fun]).newInstance(f.copy()) 
}

case class Lambda(val params: Array[Param], f: Fun) extends FPattern {
  def isGenerable() = true
  override def copy() = new Lambda(params, f)
}

object Lambda {
  def apply(f: (Param) => Fun) = {
    val params = Array(Param(Var(""), UndefType))
    new Lambda(params, f(params(0)))
  }

  // TODO: Think if multiple parameters are really necessary
  /*
  def apply(f: (Param, Param) => Fun) = {
    val params = Array(Param(Var(""), UndefType), Param(Var(""), UndefType))
    new Lambda(params, f(params(0), params(1)))
  }
  */
}

abstract class AbstractMap(f:Fun) extends FPattern
object AbstractMap {
	def unapply(am: AbstractMap): Option[Fun] = Some(am.f)
}

case class Map(f:Fun) extends AbstractMap(f)  {
  def isGenerable() = false
}
object Map {
  def apply(f: Fun, input: Fun) = new Map(f) o input
}

abstract class GenerableMap(f:Fun) extends  AbstractMap(f) {
    def isGenerable() = true
}

abstract class AbstractReduce(f:Fun, val init: Value) extends FPattern
object AbstractReduce {
	def unapply(ar: AbstractReduce): Option[(Fun, Value)] = Some(ar.f, ar.init)
}
case class Reduce(f: Fun, override val init: Value) extends AbstractReduce(f, init) {
    def isGenerable() = false    
}

case class PartRed(f: Fun, init: Value) extends FPattern {
      def isGenerable() = false
}

case class Join() extends Pattern() {
   def isGenerable() = true
   override def copy() = Join()    
}
case class Split(val chunkSize: Expr) extends Pattern() {
   def isGenerable() = true
   override def copy() = Split(chunkSize)  
}

case class asScalar() extends Pattern() {
   def isGenerable() = true
   override def copy() = asScalar() 
}
case class asVector(val len: Expr) extends Pattern() {
   def isGenerable() = true
   override def copy() = asVector(len)    
}

/*
// TODO: disuss if this should be a Fun again (if so, this has to be replaced in the very first pass before type checking)
case class Vectorize(n: Expr, f: Fun) extends FPattern {
  def isGenerable() = true
  override def copy() = Vectorize(n, f)
}
*/

object Vectorize {
  class Helper(n: Expr) {
    def apply(uf: UserFun): UserFun = {
      UserFun.vectorize(uf, n)
    }

    def apply(v: Value): Value = {
      Value.vectorize(v, n)
    }
  }

  def apply(n: Expr): Helper = {
    new Helper(n)
  }
}

case class UserFun(val name: String, val paramNames: Any, val body: String,
                   val expectedInT: Type, val expectedOutT: Type) extends Fun() {
  override def isGenerable() = true
  override def copy() = UserFun(name, paramNames, body, expectedInT, expectedOutT)

  override def toString() = "UserFun("+ name + ")" // for debug purposes
}

object UserFun {
  def vectorize(uf: UserFun, n: Expr): UserFun = {
    val name = uf.name + n
    val expectedInT = Type.vectorize(uf.expectedInT, n)
    val expectedOutT = Type.vectorize(uf.expectedOutT, n)

    // create new user fun
    UserFun(name, uf.paramNames, uf.body, expectedInT, expectedOutT)
  }
}

case class Input(val variable: Var, var expectedOutT: Type) extends Fun() {
  this.inT = NoType
  override def isGenerable() = true
  override def copy() = Input(variable, expectedOutT)    
}

case class Param(val variable: Var, var expectedOutT: Type) extends Fun() {
  this.inT = NoType
  override def isGenerable() = true
  override def copy() = Input(variable, expectedOutT)
}

case class Value(value: String, expectedOutT: Type) extends Fun() {
  this.inT = NoType
  override def isGenerable() = true
  override def copy() = Value(value, expectedOutT)
}

object Value {
  implicit def IntToValue(i: Int) = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float) = Value(f.toString + "f", opencl.ir.Float)

  def vectorize(v: Value, n: Expr): Value = {
    Value(v.value, Type.vectorize(v.expectedOutT, n))
  }
}

case class Iterate(n: Expr, f: Fun) extends FPattern() {
  override def isGenerable() = true
  override def copy() = Iterate(n, f)
  var swapBuffer: Memory = UnallocatedMemory
}

object Iterate {
  def apply(n: Expr): ((Fun) => Iterate)  = (f: Fun) => Iterate(n ,f)

  def varName(): String = {
    "iterSize"
  }
}

case class Zip(f1: Fun, f2: Fun) extends Fun() {
  override def isGenerable() = true
  override def copy() = Zip(f1, f2)
}

object Zip {
  def apply(f1: Fun): ((Fun) => Zip) = (f2: Fun) => Zip(f1, f2)
}