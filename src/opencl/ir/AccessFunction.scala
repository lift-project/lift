package opencl.ir

import ir._

class AccessFunction(val f: (ArithExpr) => ArithExpr, val scope: String) {
  def apply(e: ArithExpr): ArithExpr = f(e)
}

class ReorderAccessFunction(override val f: (ArithExpr) => ArithExpr, override val scope: String) extends AccessFunction(f, scope)

object ReorderAccessFunction {
  def apply(f: (ArithExpr) => ArithExpr, scope: String) = new ReorderAccessFunction(f, scope)
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
    assert(expr.outT != UndefType)

    expr match {
      case pr: ParamReference =>
        pr.inAccess = getAccessAtIndex(pr.p.inAccess, pr.i)
        pr.inAccess
      case p: Param => p.inAccess
      case call: FunCall =>
        addAccessFunctionsFunCall(call, outputAccess)
    }
  }

  private def getInAccessFromArgs(call: FunCall, outputAccess: AccessFunctions): AccessFunctions = {
    if (call.args.isEmpty) {
      EmptyAccessFuntions
    } else if (call.args.length == 1) {
      addAccessFunctions(call.args(0), outputAccess)
      call.args(0).inAccess
    } else {
      call.args.map(addAccessFunctions(_, outputAccess))
      val inAfs = call.args.map(_.inAccess)

      AccessFunctionsCollection( inAfs:_*  )
    }
  }

  private def addAccessFunctionsFunCall(call: FunCall, outputAccess: AccessFunctions): AccessFunctions = {
    assert(call.inM != UnallocatedMemory && call.outM != UnallocatedMemory)

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
          case f: FPattern => addAccessFunctionsFPattern(f, inAccess, outputAccess)
          case cf: CompFunDef =>
            cf.funs.foldRight[AccessFunctions](inAccess)(
              (lambda: Lambda, af: AccessFunctions) => {
                if (lambda.params.length != 1) throw new NumberOfArgumentsException
                lambda.params(0).inAccess = af
                addAccessFunctions(lambda.body, outputAccess)
              })
            outputAccess // next input

          case z: Zip => addAccessFunctionsZip(z, call, inAccess, outputAccess)

          case Split(_) | Join() | asVector(_) | asScalar()  =>
            call.inAccess = inAccess
            inAccess // next input

          case uf: UserFunDef =>
            call.inAccess = inAccess
            call.outAccess = outputAccess
            call.outAccess // next input

          case r: ReorderStride =>
            call.inAccess = addAccessFunctionsReorderStride(call, inAccess)
            call.inAccess
        }
    }
  }

  private def addAccessFunctionsZip(z: Zip, call: FunCall, inAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    inAccess match {
      case coll: AccessFunctionsCollection =>
        if (coll.elems.length != 2) throw new NumberOfArgumentsException
        call.inAccess = coll
        call.outAccess = outputAccess
        coll // next input
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def addAccessFunctionsFPattern(f: FPattern, inAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    if (f.f.params.length != 1) throw new NumberOfArgumentsException
    f.f.params(0).inAccess = inAccess

    addAccessFunctions(f.f.body, outputAccess)

    outputAccess // next input
  }

  private def addAccessFunctionsLambda(l: Lambda, call: FunCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).inAccess = inputAccess
    } else {
      val coll = inputAccess match { case coll: AccessFunctionsCollection => coll}
      if (l.params.length != coll.elems.length) throw new NumberOfArgumentsException

      (l.params zip coll.elems).map({case (p, af) => p.inAccess = af})
    }
    addAccessFunctions(l.body, outputAccess)

    outputAccess // next input
  }

  private def addAccessFunctionsMap(call: MapCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    val m = call.f

    // input
    val inAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    call.inAccess = updateAccessFunction(inputAccess, Type.length(call.inT).head) :+ inAccessFun

    // output
    val outAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    call.outAccess = updateAccessFunction(outputAccess, Type.length(call.outT).head) :+ outAccessFun

    // recurse
    if (m.f.params.length != 1) throw new NumberOfArgumentsException
    m.f.params(0).inAccess = call.inAccess
    addAccessFunctions(call.f.f.body, call.outAccess)

    removeReorderAccessFunctions(outputAccess) // nextInput
  }

  private def addAccessFunctionsMapSeq(call: MapCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    val m = call.f

    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(m.f.body.inT), "MapSeq")

    // input
    val inChunkSizes = Type.length(call.inT).reduce(_ * _) // this includes the vector size
    call.inAccess = updateAccessFunction(inputAccess, inChunkSizes) :+ innermostAccess

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    call.outAccess = updateAccessFunction(removeReorderAccessFunctions(outputAccess), outChunkSizes) :+ innermostAccess

    // recurse
    if (m.f.params.length != 1) throw new NumberOfArgumentsException
    m.f.params(0).inAccess = call.inAccess
    addAccessFunctions(call.f.f.body, call.outAccess)

    removeReorderAccessFunctions(outputAccess) // nextInput
  }

  private def addAccessFunctionsReduce(call: ReduceCall, inputAccess: AccessFunctions, outputAccess: AccessFunctions): AccessFunctions = {
    val r = call.f

    val inT = call.inT match { case tt: TupleType => tt.elemsT(1) }

    val initA = inputAccess match { case coll: AccessFunctionsCollection => coll.elems(0) }
    val inA = inputAccess match { case coll: AccessFunctionsCollection => coll.elems(1) }

    val t = call.f.f.body.inT match { case tt: TupleType => tt.elemsT(1) } // type of the second argument
    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(t), "ReduceSeq")

    // input
    val inChunkSizes = Type.length(inT).reduce(_ * _) // this includes the vector size
    //call.inAccess = AccessFunctionsCollection(initA, updateAccessFunction(inA, inChunkSizes) :+ innermostAccess)
    if (r.f.params.length != 2) throw new NumberOfArgumentsException
    r.f.params(0).inAccess = initA
    r.f.params(1).inAccess = updateAccessFunction(inA, inChunkSizes) :+ innermostAccess

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    call.outAccess = updateAccessFunction(removeReorderAccessFunctions(outputAccess), outChunkSizes)

    // recurse
    addAccessFunctions(call.f.f.body, call.outAccess)

    removeReorderAccessFunctions(outputAccess) // nextInput
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

  private def addAccessFunctionsReorderStride(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    call.inAccess = inputAccess

    val s = Type.getLength(call.inT)
    val n = Type.getLength(Type.getElemT(call.inT))

    val scope = getLatestScope(inputAccess)

    inputAccess :+ ReorderAccessFunction( (i:ArithExpr) => { i / n + s * (i % n) } , scope)
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


