package ir

import ir.ast._
import opencl.ir.pattern._

class Context extends Cloneable {

  var mapDepth : Int = 0
  var inMapGlb  = Seq(false, false, false)
  var inMapWrg  = Seq(false, false, false)
  var inMapLcl  = Seq(false, false, false)
  var inMapWarp = false
  var inMapLane = false
  var inMapSeq  = false
  var inReduceSeq = false

  def incMapDepth() : Context = {
    val c = this.copy()
    c.mapDepth += 1
    c
  }
  
  def setInMapGlb(dim: Int) : Context = {
    val c = this.copy()
    c.inMapGlb = c.inMapGlb.updated(dim, true)
    c
  }  
  
  def setInMapWrg(dim: Int) : Context = {
    val c = this.copy()
    c.inMapWrg = c.inMapWrg.updated(dim, true)
    c
  }
   
   def setInMapLcl(dim: Int) : Context = {
    val c = this.copy()
    c.inMapLcl = c.inMapLcl.updated(dim, true)
    c
  }

  def setInMapWarp() : Context = {
    val c = this.copy()
    c.inMapWarp = true
    c
  }

  def setInMapLane() : Context = {
    val c = this.copy()
    c.inMapLane = true
    c
  }

  def setInMapSeq() : Context = {
    val c = this.copy()
    c.inMapSeq = true
    c
  }

  def setInReduceSeq() : Context = {
    val c = this.copy()
    c.inReduceSeq = true
    c
  }
  
  def copy(): Context =
    this.clone().asInstanceOf[Context]
}

object Context {

  def apply(lambda: Lambda): Unit = updateContext(lambda.body)

   /**
     * Update the context recursively
     */
  def updateContext(expr: Expr): Unit = updateContext(expr, new Context)
    
  /**
   * Update the context recursively
   */  
  def updateContext(expr: Expr, ctx: Context): Unit = {
    if (ctx != null) {
      expr.context = ctx
      expr match {
        case call: FunCall =>
          call.args.foreach(arg => updateContext(arg, ctx.copy()))

          call.f match {
            case Map(inF)      =>
              updateContext(inF.body, ctx.incMapDepth())
            case MapSeq(inF)   =>
              updateContext(inF.body, ctx.incMapDepth().setInMapSeq())
            case MapGlb(dim,inF) =>
              updateContext(inF.body, ctx.incMapDepth().setInMapGlb(dim))
            case MapWrg(dim,inF) =>
              updateContext(inF.body, ctx.incMapDepth().setInMapWrg(dim))
            case MapAtomWrg(dim, inF, _) =>
              updateContext(inF.body, ctx.incMapDepth().setInMapWrg(dim))
            case MapLcl(dim,inF) =>
              updateContext(inF.body, ctx.incMapDepth().setInMapLcl(dim))
            case MapWarp(inF)  =>
              updateContext(inF.body, ctx.incMapDepth().setInMapWarp())
            case MapLane(inF)  =>
              updateContext(inF.body, ctx.incMapDepth().setInMapLane())
            case ReduceSeq(inF) =>
              updateContext(inF.body, ctx.setInReduceSeq())

            case fp: FPattern => updateContext(fp.f.body, ctx.copy())

            case l: Lambda => updateContext(l.body, ctx.copy())

            case _ =>
          }
        case _ =>
      }
    }
  }
}
