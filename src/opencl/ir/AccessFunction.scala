package opencl.ir

import ir._

class AccessFunction(val f: (ArithExpr) => ArithExpr, val scope: String) {
  def apply(e: ArithExpr): ArithExpr = f(e)
}

object AccessFunction {
  def apply(f: (ArithExpr) => ArithExpr, scope: String) = new AccessFunction(f, scope)

  def id(scope: String) = new AccessFunction((e: ArithExpr) => e, scope)

  def addAccessFunctions(expr: Expr): (Array[AccessFunction], Array[AccessFunction]) = {
    addAccessFunctions(expr, (Array(), Array()))
  }

  private def addAccessFunctions(expr: Expr,
                                 access: (Array[AccessFunction], Array[AccessFunction])): (Array[AccessFunction], Array[AccessFunction]) = {
    assert(expr.outT != UndefType)
    assert(expr.inM != UnallocatedMemory && expr.outM != UnallocatedMemory)

    val (inputAccess, outputAccess) = access

    expr match {
      case call: MapCall => call.f match {
        case m: MapSeq => addAccessFunctionsMapSeq(call, access)
        case m: AbstractMap => addAccessFunctionsMap(call, access)
      }
      case call: ReduceCall => addAccessFunctionsReduce(call, access)
      case call: FunCall =>
        call.inAccess = inputAccess
        call.outAccess = outputAccess

        call.f match {
          case l: Lambda => addAccessFunctions(l.body, access)
          case f: FPattern => addAccessFunctions(f.f.body, access)

          case cf: CompFunDef =>
            cf.funs.foldRight[Option[(Array[AccessFunction], Array[AccessFunction])]](None)(
              (lambda: Lambda, af: Option[(Array[AccessFunction], Array[AccessFunction])]) => lambda.body match {
                case call: FunCall => call.f match {
                  // access functions are usually only passed into nested functions!
                  // the only exception (so far) is reorder
                  case r: ReorderStride => Some(addAccessFunctionsReorderStride(call, access))

                  case _ =>
                    if (af.isDefined) {
                      addAccessFunctions(lambda.body, af.get)
                    } else {
                      addAccessFunctions(lambda.body, access)
                    }
                    None // access function is consumed and not passed to the next function in line
                }
              })
            access

          case _ => access
        }
    }
  }

  private def addAccessFunctionsMap(call: MapCall,
                                    access: (Array[AccessFunction], Array[AccessFunction])): (Array[AccessFunction], Array[AccessFunction]) = {
    val (inputAccess, outputAccess) = access

    // input
    val inAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    call.inAccess = updateAccessFunction(inputAccess, Type.length(call.inT).head) :+ inAccessFun

    // output
    val outAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    call.outAccess = updateAccessFunction(outputAccess, Type.length(call.outT).head) :+ outAccessFun

    // recurse
    addAccessFunctions(call.f.f.body, (call.inAccess, call.outAccess))

    access
  }

  private def addAccessFunctionsMapSeq(call: MapCall,
                                       access: (Array[AccessFunction], Array[AccessFunction])): (Array[AccessFunction], Array[AccessFunction]) = {
    val (inputAccess, outputAccess) = access

    val m = call.f

    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(m.f.body.inT), "MapSeq")

    // input
    val inChunkSizes = Type.length(call.inT).reduce(_ * _) // this includes the vector size
    call.inAccess = updateAccessFunction(inputAccess, inChunkSizes) :+ innermostAccess

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    val updatedOutAccessFuns = updateAccessFunction(outputAccess, outChunkSizes)
    call.outAccess = updatedOutAccessFuns :+ innermostAccess

    // recurse
    addAccessFunctions(call.f.f.body, (call.inAccess, updatedOutAccessFuns))

    access
  }

  private def addAccessFunctionsReduce(call: ReduceCall,
                                       access: (Array[AccessFunction], Array[AccessFunction])): (Array[AccessFunction], Array[AccessFunction]) = {
    val (inputAccess, outputAccess) = access

    val inT = call.inT match { case tt: TupleType => tt.elemsT(1) }

    val t = call.f.f.body.inT match { case tt: TupleType => tt.elemsT(1) } // type of the second argument
    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(t), "ReduceSeq")

    // input
    val inChunkSizes = Type.length(inT).reduce(_ * _) // this includes the vector size
    call.inAccess = updateAccessFunction(inputAccess, inChunkSizes) :+ innermostAccess

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    call.outAccess = updateAccessFunction(outputAccess, outChunkSizes)

    // recurse
    addAccessFunctions(call.f.f.body, (call.inAccess, call.outAccess))

    access
  }


  private def addAccessFunctionsReorderStride(call: FunCall,
                                              access: (Array[AccessFunction], Array[AccessFunction])): (Array[AccessFunction], Array[AccessFunction]) = {
    val (inputAccess, outputAccess) = access

    val s = Type.getLength(call.inT)
    val n = Type.getLength(Type.getElemT(call.inT))
    call.inAccess = inputAccess :+ AccessFunction( (i:ArithExpr) => { i / n + s * (i % n) } , inputAccess.last.scope)

    call.outAccess = outputAccess

    (call.inAccess, call.outAccess)
  }


  private def updateAccessFunction(access: Array[AccessFunction], chunkSize: ArithExpr) = {
    access.map( {
      case ma: MapAccessFunction => MapAccessFunction.multWithChunkSize(ma, chunkSize)
      case af: AccessFunction => af
    } )
  }

}

class MapAccessFunction(val loopVar: Var, val chunkSize: ArithExpr, val mapName: String)
  extends AccessFunction((i: ArithExpr) => (loopVar * chunkSize) + i, mapName)

object MapAccessFunction {
  def apply(loopVar: Var, initialChunkSize: ArithExpr, mapName: String) =
    new MapAccessFunction(loopVar, initialChunkSize, mapName)

  def multWithChunkSize(ma: MapAccessFunction, chunkSize: ArithExpr): MapAccessFunction = {
    MapAccessFunction(ma.loopVar, ma.chunkSize * chunkSize, ma.mapName)
  }

  def asMapAccessFunction(a: AccessFunction): MapAccessFunction = {
    a match {
      case ma: MapAccessFunction => ma
      case _ => throw new IllegalArgumentException
    }
  }
}


