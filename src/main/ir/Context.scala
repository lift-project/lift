package ir

import ir.ast._
import opencl.ir.ast._
import opencl.ir.pattern._

class Context extends Cloneable {

  // TODO(tlutz) keep a stack to track current dim
  var mapDepth : Int = 0
  var inMapGlb = false
  var inMapWrg = false
  var inMapLcl = false
  var inMapWarp = false
  var inMapLane = false
  var inSeq = false
       
  def incMapDepth() : Context = {
    val c = this.copy()
    c.mapDepth += 1
    c
  }
  
  def setInMapGlb() : Context = {
    val c = this.copy()
    c.inMapGlb = true
    c
  }  
  
  def setInMapWrg() : Context = {
    val c = this.copy()
    c.inMapWrg = true
    c
  }
   
   def setInMapLcl() : Context = {
    val c = this.copy()
    c.inMapLcl = true
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

  def setInSeq() : Context = {
    val c = this.copy()
    c.inSeq = true
    c
  }


  /*override def toString(): String = {
    "Contex[mapDepth="+mapDepth+"]"
  }*/
  
  def copy() = this.clone().asInstanceOf[Context]
}

object Context {
   /**
     * Update the context recursively
     */
  def updateContext(expr: Expr): Unit = updateContext(expr, expr.context)
    
  /**
   * Update the context recursively
   */  
  def updateContext(expr: Expr, ctx: Context): Unit = {
    if (ctx != null) {
      expr.context = ctx
      expr match {
        case call: FunCall => call.f match {

          case Map(inF)    => updateContext(inF.body, ctx.incMapDepth())
          case MapSeq(inF) => updateContext(inF.body, ctx.incMapDepth().setInSeq())
          case MapGlb(_,inF) => updateContext(inF.body, ctx.incMapDepth().setInMapGlb())
          case MapWrg(_,inF) => updateContext(inF.body, ctx.incMapDepth().setInMapWrg())
          case MapLcl(_,inF) => updateContext(inF.body, ctx.incMapDepth().setInMapLcl())
          case MapWarp(inF) => updateContext(inF.body, ctx.incMapDepth().setInMapWarp())
          case MapLane(inF) => updateContext(inF.body, ctx.incMapDepth().setInMapLane())
          case ReduceSeq(inF) => updateContext(inF.body, ctx.setInSeq())

          case fp: FPattern => updateContext(fp.f.body, ctx.copy())
          case cf: CompFun => cf.funs.foreach(inF => updateContext(inF.body, ctx.copy()))
          case _ =>
        }
        case _ =>
      }
    }    
  }
}