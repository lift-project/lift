package ir


sealed class FunExpr(val f : FunDef, val args : FunExpr*) {
  var context : Context = null

  // type information
  var inT: Type = UndefType
  var outT: Type = UndefType

  // memory information
  var inM: Memory = UnallocatedMemory
  var outM: Memory = UnallocatedMemory

  
  def setContext(ctx: Context): FunExpr = {
    if (ctx != null)
      this.context = ctx
    this
  }

  def copy: FunExpr = {
    //val c = new FunExpr(this.f, this.args:_*)
    //c.context = this.context
    //c.inT = this.inT
    //c.ouT = this.ouT
    this.clone().asInstanceOf[FunExpr]
  }

  override def toString = {
    val fS = if (f == null) { "null" } else { f.toString }
    val argS = if (args.length > 0) args.map(_.toString).reduce(_ + ", " + _) else ""

    fS + "(" + argS + ")"
  }


}

case class Param() extends FunExpr(null) {
  override def toString = "PARAM"
}

object Param {
  def apply(outT: Type): Param = {
    val p = Param()
    p.outT =outT
    p
  }
}

case class Value(value: String) extends FunExpr(null)

object Value {
  def apply(value: String, outT: Type): Value = {
    val v = Value(value)
    v.outT = outT
    v
  }

  implicit def IntToValue(i: Int) = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float) = Value(f.toString + "f", opencl.ir.Float)

  def vectorize(v: Value, n: Expr): Value = {
    Value(v.value, Type.vectorize(v.outT, n))
  }
}


object FunExpr {

  
  def visit[T](z:T)(f: FunExpr, vfn: (FunExpr,T) => T): T = {
    val result = vfn(f,z)
    f.f match {
      case fp: FPattern => visit(result)(fp.f.body, vfn)
      case cf: CompFunDef => cf.funs.foldRight(result)((inF,x)=>visit(x)(inF.body, vfn))
      case _ => result
    }
  }
  
  /*
   * Visit the expression of function f (recursively) and rebuild along the way
   */
  def visitExpr(f: FunExpr, exprF: (Expr) => (Expr)) : FunExpr = {
    visit(f, (inF:FunExpr) => inF.f match {
      case Split(e) => Split(exprF(e))(inF.args:_*)
      case asVector(e) => asVector(exprF(e))(inF.args:_*)
      case _ => inF
    }, (inF:FunExpr) => inF)
  }
  
  /*
   * Visit the function f recursively and rebuild along the way
   */
   def visit(f: FunExpr, pre: (FunExpr) => (FunExpr), post: (FunExpr) => (FunExpr)) : FunExpr = {
    var newF = pre(f)
    newF = newF.f match {
      case cf: CompFunDef => CompFunDef(cf.funs.map(inF => visit(inF.body, pre, post).f).map(Lambda.FunDefToLambda(_)):_*)(newF.args:_*)
      case fp: FPattern => fp.getClass().getConstructor(classOf[FunExpr]).newInstance(visit(fp.f.body,pre,post))(newF.args:_*)
      case _ => newF.copy
    }
    post(newF)
  }

  /*
   * Visit the function f recursively
   */
  def visit(f: FunExpr, pre: (FunExpr) => (Unit), post: (FunExpr) => (Unit)): Unit = {
    pre(f)
    f.f match {
      case fp: FPattern => visit(fp.f.body, pre, post)
      case cf: CompFunDef => cf.funs.reverseMap(inF => visit(inF.body, pre, post))
      case _ =>
    }
    post(f)
  }
   
}
