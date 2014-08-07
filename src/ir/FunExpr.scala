package ir


sealed class FunExpr(val f : FunDef, val args : FunExpr*) {
  var context : Context = null

  // type information
  var inT: Type = UndefType
  var ouT: Type = UndefType

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


}

case class Param(var outT: Type) extends FunExpr(null)
case class Value(value: String, outT: Type) extends FunExpr(null)

object Value {
  implicit def IntToValue(i: Int) = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float) = Value(f.toString + "f", opencl.ir.Float)

  def vectorize(v: Value, n: Expr): Value = {
    Value(v.value, Type.vectorize(v.outT, n))
  }
}


object FunExpr {

  def replaceRef(f: FunExpr, oriF: FunExpr, newF: FunExpr) : FunExpr = {

    //println("f: "+f+" oriF: "+oriF+" newF: "+newF)

    if (f.eq(oriF))
      return newF

    new FunExpr(f.f, f.args.map(arg => replaceRef(arg, oriF, newF)) :_*)

    /*f.f match {
      case cf: CompFunDef => CompFunDef(cf.funs.zip(f.args)map(inF => replaceRef(inF, oriF, newF)):_*)(f.args)
      case fp: FPattern => fp.getClass().getConstructor(classOf[FunExpr]).newInstance(replaceRef(fp.f,oriF,newF))
      case _ => f//f.copy()
    }*/
  }
  
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
