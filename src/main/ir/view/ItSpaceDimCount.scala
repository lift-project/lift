package ir.view

import ir.ast.{AbstractMap, Expr, FunCall, IRNode}
import opencl.ir.pattern._


object ItSpaceDimCount {

  def apply(e: Expr) : Int = {

    var wrgDepth = 0
    var lclOrGlbDepth = 0
    var maxWrgDepth = 0
    var maxLclOrGlbDepth = 0

    def prePost(n: IRNode) : (IRNode => Unit) = {
      n match {
        case am:AbstractMap =>
          am match {
                case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl =>
                  lclOrGlbDepth+=1
                  if (lclOrGlbDepth > maxLclOrGlbDepth)
                    maxLclOrGlbDepth = lclOrGlbDepth
                  (_) => {lclOrGlbDepth-=1}
                case _: MapWrg =>
                  wrgDepth+=1
                  if (wrgDepth > maxWrgDepth)
                    maxWrgDepth = wrgDepth
                  (_) => { wrgDepth-=1}
                case _ =>
                   (_) => {}
              }
        case _=>(_) => {}
      }
    }

    IRNode.visitPrePost(e, prePost)

    math.max(maxWrgDepth,maxLclOrGlbDepth)
  }

}
