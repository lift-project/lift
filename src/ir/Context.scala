package ir

import opencl.ir._

class Context extends Cloneable {
  
  var mapDepth : Int = 0
  var inMapGlb = false;
  var inMapWrg = false
  var inMapLcl = false
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
   /*
   * Update the context recursively
   */
  def updateContext(f: Fun): Unit = updateContext(f, f.context)
    
  /*
   * Update the context recursively
   */  
  def updateContext(f: Fun, ctx: Context): Unit = {
    if (ctx != null) {
      f.context = ctx;
      f match {   
        
        case Map(inF)    => updateContext(inF, ctx.incMapDepth)
        case MapSeq(inF) => updateContext(inF, ctx.incMapDepth.setInSeq())
        case MapGlb(inF) => updateContext(inF, ctx.incMapDepth.setInMapGlb)
        case MapWrg(inF) => updateContext(inF, ctx.incMapDepth.setInMapWrg)
        case MapLcl(inF) => updateContext(inF, ctx.incMapDepth.setInMapLcl)
        case ReduceSeq(inF,_) => updateContext(inF, ctx.setInSeq())

        case fp: FPattern => updateContext(fp.f, ctx.copy)
        case cf: CompFun => cf.funs.map(inF => updateContext(inF, ctx.copy))
        case _ => 
      }
    }    
  }
}