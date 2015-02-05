package opencl.ir

import ir._

class AccessFunction(val f: (ArithExpr) => ArithExpr, val scope: String) {
  def apply(e: ArithExpr): ArithExpr = f(e)
}

class ReorderAccessFunction(override val f: (ArithExpr) => ArithExpr, override val scope: String) extends AccessFunction(f, scope)

object ReorderAccessFunction {
  def apply(f: (ArithExpr) => ArithExpr, scope: String) = new ReorderAccessFunction(f, scope)
}

class TransposeAccessFunction(val dim0: ArithExpr, val dim1: ArithExpr, override val f: (ArithExpr) => ArithExpr, override val scope: String) extends AccessFunction(f, scope)

object TransposeAccessFunction {
  def apply(dim0: ArithExpr, dim1: ArithExpr, f: (ArithExpr) => ArithExpr, scope: String) = new TransposeAccessFunction(dim0, dim1, f, scope)
}

object IdAccessFunction extends AccessFunction((e: ArithExpr) => e, "") {
  override def toString = "IdAccessFunction"
}

class AccessFunctions(val afs: Array[AccessFunction]) {
  def :+(af: AccessFunction): AccessFunctions = {
    new AccessFunctions(this.afs :+ af)
  }

  def ++(that: AccessFunctions): AccessFunctions = {
    new AccessFunctions(this.afs ++ that.afs)
  }
}

class AccessFunctionsCollection(val elems: AccessFunctions*) extends AccessFunctions(Array()) {
  override def :+(af: AccessFunction): AccessFunctionsCollection = {
    AccessFunctionsCollection( elems.map(_ :+ af):_* )
  }

  override def ++(that: AccessFunctions): AccessFunctionsCollection = {
    AccessFunctionsCollection( elems.map( _ ++ that ):_* )
  }
}

object AccessFunctionsCollection {
  def apply(elems: AccessFunctions*): AccessFunctionsCollection = {
    new AccessFunctionsCollection(elems:_* )
  }
}

object AccessFunctions {
  implicit def ArrayToAccessFunctions(afs: Array[AccessFunction]) = new AccessFunctions(afs)
}

object EmptyAccessFuntions extends AccessFunctions(Array()) {
  override def toString = "EmptyAccessFunctions"
}

object IdAccessFunctions extends AccessFunctions(Array(IdAccessFunction)) {
  override def toString = "IdAccessFunctions"
}

object AccessFunction {
  def apply(f: (ArithExpr) => ArithExpr, scope: String) = new AccessFunction(f, scope)

  // predefined reorder functions ...
  val transpose = (i: ArithExpr, t: Type) => {
    val outerType = t match { case at: ArrayType => at }
    val innerType = outerType.elemT match { case at: ArrayType => at }

    val outerSize = outerType.len
    val innerSize = innerType.len

    val elemSize = Type.getLengths(Type.getElemT(innerType)).reduce(_ * _)

    val col = (((i/elemSize) % innerSize) * outerSize) * elemSize
    val row = ((i/elemSize) / innerSize) * elemSize

    // TODO: simplify this ...
    row + col + (i % elemSize)
  }

  val reorderStride = (i: ArithExpr, t: Type) => {
    val s = Type.getLength(t)
    val n = Type.getLength(Type.getElemT(t))

    i / n + s * ( i % n)
  }

  val reverse = (i: ArithExpr, t: Type) => {
    val n = Type.getLength(t)

    n - 1 - i
  }

  def addAccessFunctions(expr: Expr): AccessFunctions = {
    addAccessFunctions(expr, EmptyAccessFuntions)
  }

  private def getAccessAtIndex(af: AccessFunctions, index: Int): AccessFunctions = {
    af match {
      case coll: AccessFunctionsCollection =>
        assert(index < coll.elems.length)
        coll.elems(index)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def addAccessFunctions(expr: Expr, outputAccess: AccessFunctions): AccessFunctions = {
    assert(expr.t != UndefType)

    val result = expr match {
      case pr: ParamReference =>  getAccessAtIndex(pr.p.access, pr.i)
      case p: Param =>            p.access
      case call: FunCall =>       addAccessFunctionsFunCall(call, outputAccess)
    }
    expr.access = result
    result
  }

  private def getInAccessFromArgs(call: FunCall, outputAccess: AccessFunctions): AccessFunctions = {
    if (call.args.isEmpty) {
      EmptyAccessFuntions
    } else if (call.args.length == 1) {
      addAccessFunctions(call.args(0), outputAccess)
    } else {
      AccessFunctionsCollection( call.args.map(addAccessFunctions(_, outputAccess)):_*  )
    }
  }

  private def addAccessFunctionsFunCall(call: FunCall, outputAccess: AccessFunctions): AccessFunctions = {
    assert(call.mem != UnallocatedMemory)

    // get the input access of f from the input arguments
    val inAccess = getInAccessFromArgs(call, outputAccess)

    call match {
      case call: MapCall => call.f match {
        case m: MapSeq => addAccessFunctionsMapSeq(call, inAccess, outputAccess)
        case m: AbstractMap => addAccessFunctionsMap(call, inAccess, outputAccess)
      }
      case call: ReduceCall => addAccessFunctionsReduce(call, inAccess, outputAccess)
      case call: FunCall =>
        call.f match {
          case l: Lambda => addAccessFunctionsLambda(l, call, inAccess, outputAccess)
          case cf: CompFunDef => addAccessFunctionsCompFunDef(cf, inAccess, outputAccess)

          case z: Zip => addAccessFunctionsZip(z, call, inAccess, outputAccess)

          case Split(_) | SplitDim2(_) | Join() | JoinDim2() | asVector(_) | asScalar() | Unzip() | Swap()  =>
            inAccess // next input

          case Transpose() => addAccessFunctionsTranspose(call, inAccess)

          case uf: UserFunDef => outputAccess

          case r: ReorderStride => addAccessFunctionsReorderStride(call, inAccess)

          case g: Gather => addGather(g, call, inAccess, outputAccess)

          case s: Scatter => addScatter(s, call, inAccess, outputAccess)

          case f: FPattern => addAccessFunctionsFPattern(f, inAccess, outputAccess)
        }
    }
  }

  private def addAccessFunctionsZip(z: Zip, call: FunCall, inAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    inAccess match {
      case coll: AccessFunctionsCollection =>
        if (coll.elems.length < 2) throw new NumberOfArgumentsException
        coll // next input
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def addAccessFunctionsFPattern(f: FPattern, inAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    if (f.f.params.length != 1) throw new NumberOfArgumentsException
    f.f.params(0).access = inAccess

    addAccessFunctions(f.f.body, outputAccess)

    outputAccess // next input
  }

  private def addAccessFunctionsLambda(l: Lambda, call: FunCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).access = inputAccess
    } else {
      val coll = inputAccess match { case coll: AccessFunctionsCollection => coll}
      if (l.params.length != coll.elems.length) throw new NumberOfArgumentsException

      (l.params zip coll.elems).map({case (p, af) => p.access = af})
    }
    addAccessFunctions(l.body, outputAccess)

    outputAccess // next input
  }

  private def addAccessFunctionsCompFunDef(cf: CompFunDef, inAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    cf.funs.foldRight[AccessFunctions](inAccess)(
      (lambda: Lambda, af: AccessFunctions) => {
        if (lambda.params.length != 1) throw new NumberOfArgumentsException
        lambda.params(0).access = af
        addAccessFunctions(lambda.body, outputAccess)
      })
    outputAccess // next input
  }

  private def addAccessFunctionsMap(call: MapCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    val m = call.f

    // input
    val inAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    val inAccess = updateAccessFunction(inputAccess, Type.length(call.argsType).head) :+ inAccessFun

    // output
    val outAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    val outAccess = updateAccessFunction(outputAccess, Type.length(call.t).head) :+ outAccessFun

    // recurse
    if (m.f.params.length != 1) throw new NumberOfArgumentsException
    m.f.params(0).access = inAccess
    addAccessFunctions(call.f.f.body, outAccess)

    removeReorderAccessFunctions(outputAccess) // nextInput
  }

  private def addAccessFunctionsMapSeq(call: MapCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    val m = call.f
    val funCall = m.f.body match { case call: FunCall => call }

    val innermostAccess = MapAccessFunction(call.loopVar, Type.getVectorSize(funCall.argsType), call.name)

    // input
    val inChunkSizes = Type.length(call.argsType).reduce(_ * _) // this includes the vector size
    val inAccess = updateAccessFunction(inputAccess, inChunkSizes) :+ innermostAccess

    // output
    val outChunkSizes = Type.length(call.t).reduce(_ * _)  // this includes the vector size
    val outAccess = updateAccessFunction(removeReorderAccessFunctions(outputAccess), outChunkSizes) :+ innermostAccess

    // recurse
    if (m.f.params.length != 1) throw new NumberOfArgumentsException
    m.f.params(0).access = inAccess
    addAccessFunctions(call.f.f.body, outAccess)

    removeReorderAccessFunctions(outputAccess) // nextInput
  }

  private def addAccessFunctionsReduce(call: ReduceCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    val r = call.f
    inputAccess match {
      case coll: AccessFunctionsCollection =>
        if (coll.elems.length != 2) throw new NumberOfArgumentsException
        val initA = coll.elems(0)
        val elemA = coll.elems(1)

        val innerFunCall = r.f.body match { case call: FunCall => call }

        val t = innerFunCall.args(1).t // type of the second argument of the inner function
        val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(t), "ReduceSeq")

        // input
        val inChunkSizes = Type.length(call.arg1.t).reduce(_ * _) // this includes the vector size
        if (r.f.params.length != 2) throw new NumberOfArgumentsException
        r.f.params(0).access = initA
        r.f.params(1).access = updateAccessFunction(elemA, inChunkSizes) :+ innermostAccess

        // output
        val outChunkSizes = Type.length(call.t).reduce(_ * _)  // this includes the vector size
        val outAccess = updateAccessFunction(removeReorderAccessFunctions(outputAccess), outChunkSizes)

        // recurs
        addAccessFunctions(call.f.f.body, outAccess)

        removeReorderAccessFunctions(outputAccess) // nextInput

      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def getLatestScope(af: AccessFunctions): String = {
    af match {
      case coll: AccessFunctionsCollection =>
        val scopes = coll.elems.map(getLatestScope)
        val uniqueScopes = scopes.distinct
        if (uniqueScopes.length == 1)
          uniqueScopes.head
        else
          ""
      case af: AccessFunctions =>
        if (af.afs.nonEmpty)
          af.afs.last.scope
        else
          ""
    }
  }

  private def addGather(g: Gather, call: FunCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    if (g.f.params.length != 1) throw new NumberOfArgumentsException

    var scope = getLatestScope(inputAccess)
    scope = g.f.body match { case m: MapCall => m.name; case _ => scope}
    g.f.params(0).access = inputAccess :+ ReorderAccessFunction( (i: ArithExpr) => g.idx.f(i, call.argsType), scope)

    addAccessFunctions(g.f.body, outputAccess)

    outputAccess // next input
  }

  private def addScatter(s: Scatter, call: FunCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    if (s.f.params.length != 1) throw new NumberOfArgumentsException

    val scope = getLatestScope(inputAccess)
    s.f.params(0).access = inputAccess

    addAccessFunctions(s.f.body, outputAccess :+ ReorderAccessFunction( (i: ArithExpr) => s.idx.f(i, call.t), scope))

    outputAccess :+ ReorderAccessFunction( (i: ArithExpr) => s.idx.f(i, call.t), scope) // next input
  }

  private def addAccessFunctionsReorderStride(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    val s = Type.getLength(call.argsType)
    val n = Type.getLength(Type.getElemT(call.argsType))

    val scope = getLatestScope(inputAccess)

    inputAccess :+ ReorderAccessFunction( (i:ArithExpr) => { i / n + s * ( i % n) } , scope)
  }

  private def addAccessFunctionsTranspose(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    val scope = ""

    // types after the transpose of the matrix
    val outerType = call.t match { case at: ArrayType => at }
    val innerType = outerType.elemT match { case at: ArrayType => at }

    val outerSize = outerType.len
    val innerSize = innerType.len

    val elemSize = Type.getLengths(Type.getElemT(innerType)).reduce(_ * _)

    val af = TransposeAccessFunction(
      innerSize,
      outerSize,
      (i:ArithExpr) => {
        val col = (((i/elemSize) % innerSize) * outerSize) * elemSize
        val row = ((i/elemSize) / innerSize) * elemSize

        row + col + (i % elemSize)
      } , scope)

    inputAccess :+ af
  }


  private def updateAccessFunction(access: AccessFunctions, chunkSize: ArithExpr): AccessFunctions = {
    access match {
      case coll: AccessFunctionsCollection =>
        AccessFunctionsCollection( coll.elems.map( updateAccessFunction(_, chunkSize) ):_* )

      case _ =>
        access.afs.map( {
          case ma: MapAccessFunction => MapAccessFunction.multWithChunkSize(ma, chunkSize)
          case af: AccessFunction => af
        } )
    }
  }

  private def removeReorderAccessFunctions(access: AccessFunctions): AccessFunctions = {
    access match {
      case coll: AccessFunctionsCollection =>
        AccessFunctionsCollection( coll.elems.map( removeReorderAccessFunctions ):_* )
      case _ =>
        access.afs.filterNot({
          case _: ReorderAccessFunction => true
          case _: TransposeAccessFunction => true
          case _ => false
        })
    }
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


