package opencl.ir

import ir._

class AccessFunction(val f: (ArithExpr) => ArithExpr, val scope: String) {
  def apply(e: ArithExpr): ArithExpr = f(e)
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

class AccessFunctionsCollection(val elems: AccessFunctions*) extends AccessFunctions(Array())

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

  private def determineInputAccessFun(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    call.f match {
      case r: AbstractPartRed =>
        if (call.args.length != 2) throw new NumberOfArgumentsException
        val initA  = addAccessFunctions(call.args(0), EmptyAccessFuntions)
        val inputA = addAccessFunctions(call.args(1), inputAccess)
        new AccessFunctionsCollection(initA, inputA)

      case _ =>
        if (call.args.isEmpty)
          inputAccess
        else if (call.args.length == 1)
          addAccessFunctions(call.args(0), inputAccess)
        else { //more than one argument
          val inAs = inputAccess match {
            // if the input is a collection unpack it
            case coll: AccessFunctionsCollection =>
              assert(call.args.length == coll.elems.length)
              (call.args zip coll.elems).map({
                case (e, af) => addAccessFunctions(e, af)
              })

            case af: AccessFunctions => call.args.map(addAccessFunctions(_, inputAccess))
          }

          new AccessFunctionsCollection(inAs:_*)
        }

    }
  }

  private def addAccessFunctionsFunCall(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    assert(call.inM != UnallocatedMemory && call.outM != UnallocatedMemory)

    println("call: " + call)
    inputAccess match {
      case coll: AccessFunctionsCollection =>
        println("\tafs: " + coll.elems.map(_.toString).reduce(_ + ", " + _))
      case af: AccessFunctions =>
        println("\taf: " + af.toString)
    }

    // determine the input access of f based on the input arguments
    val inAccess = determineInputAccessFun(call, inputAccess)

    val out = call match {
      case call: MapCall => call.f match {
        case m: MapSeq => addAccessFunctionsMapSeq(call, inAccess)
        case m: AbstractMap => addAccessFunctionsMap(call, inAccess)
      }
      case call: ReduceCall => addAccessFunctionsReduce(call, inAccess)
      case call: FunCall =>
        call.f match {
          case uf: UserFunDef =>
            call.inAccess = inAccess
            inAccess
          case l: Lambda => addAccessFunctions(l.body, inAccess)
          case f: FPattern => addAccessFunctions(f.f.body, inAccess)
          case cf: CompFunDef =>
            cf.funs.foldRight[Option[AccessFunctions]](None)(
              (lambda: Lambda, af: Option[AccessFunctions]) => lambda.body match {
                case call: FunCall => call.f match {
                  case r: ReorderStride => Some(addAccessFunctionsReorderStride(call, inAccess))

                  case _ =>
                    if (af.isDefined) {
                      Some(addAccessFunctions(lambda.body, af.get))
                    } else {
                      Some(addAccessFunctions(lambda.body, inAccess))
                    }
                }
              })
            inAccess

          case _ =>
            inAccess
        }
    }

    println("after call: " + call)
    println(" in: ")
    call.inAccess match {
      case coll: AccessFunctionsCollection =>
        println("\tafs: " + coll.elems.map(_.toString).reduce(_ + ", " + _))
      case af: AccessFunctions =>
        println("\taf: " + af.toString)
    }
    println(" out: ")
    call.outAccess match {
      case coll: AccessFunctionsCollection =>
        println("\tafs: " + coll.elems.map(_.toString).reduce(_ + ", " + _))
      case af: AccessFunctions =>
        println("\taf: " + af.toString)
    }

    out
  }

  private def addAccessFunctions(expr: Expr, inputAccess: AccessFunctions): AccessFunctions = {
    assert(expr.outT != UndefType)

    expr match {
      case p: Param =>
        if (p.inAccess == IdAccessFunctions) {
          p.inAccess
        } else {
          p.inAccess = inputAccess
          p.inAccess
        }
      case call: FunCall => addAccessFunctionsFunCall(call, inputAccess)
    }
  }

  private def addAccessFunctionsMap(call: MapCall, inputAccess: AccessFunctions): AccessFunctions = {
    // input
    val inAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    call.inAccess = updateAccessFunction(inputAccess, Type.length(call.inT).head) :+ inAccessFun

    // output
    val outAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    call.outAccess = updateAccessFunction(inputAccess, Type.length(call.outT).head) :+ outAccessFun

    // recurse
    addAccessFunctions(call.f.f.body, call.inAccess)

    inputAccess
  }

  private def addAccessFunctionsMapSeq(call: MapCall, inputAccess: AccessFunctions): AccessFunctions = {
    val m = call.f

    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(m.f.body.inT), "MapSeq")

    // input
    val inChunkSizes = Type.length(call.inT).reduce(_ * _) // this includes the vector size
    call.inAccess = updateAccessFunction(inputAccess, inChunkSizes) :+ innermostAccess

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    call.outAccess = updateAccessFunction(removeReorderAccessFunctions(inputAccess), outChunkSizes) :+ innermostAccess

    // recurse
    addAccessFunctions(call.f.f.body, call.inAccess)

    removeReorderAccessFunctions(inputAccess)
  }

  private def addAccessFunctionsReduce(call: ReduceCall, inputAccess: AccessFunctions): AccessFunctions = {

    val inT = call.inT match { case tt: TupleType => tt.elemsT(1) }

    val initA = inputAccess match { case coll: AccessFunctionsCollection => coll.elems(0) }
    val inA = inputAccess match { case coll: AccessFunctionsCollection => coll.elems(1) }

    val t = call.f.f.body.inT match { case tt: TupleType => tt.elemsT(1) } // type of the second argument
    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(t), "ReduceSeq")

    // input
    val inChunkSizes = Type.length(inT).reduce(_ * _) // this includes the vector size
    call.inAccess = new AccessFunctionsCollection(initA, updateAccessFunction(inA, inChunkSizes) :+ innermostAccess)

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    call.outAccess = updateAccessFunction(removeReorderAccessFunctions(inA), outChunkSizes)

    // recurse
    addAccessFunctions(call.f.f.body, call.inAccess)

    removeReorderAccessFunctions(inA)
  }


  private def addAccessFunctionsReorderStride(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    call.inAccess = inputAccess

    val s = Type.getLength(call.inT)
    val n = Type.getLength(Type.getElemT(call.inT))

    inputAccess.afs :+ AccessFunction( (i:ArithExpr) => { i / n + s * (i % n) } , inputAccess.afs.last.scope)
  }


  private def updateAccessFunction(access: AccessFunctions, chunkSize: ArithExpr): AccessFunctions = {
    access match {
      case coll: AccessFunctionsCollection =>
        new AccessFunctionsCollection( coll.elems.map( updateAccessFunction(_, chunkSize) ):_* )

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
        new AccessFunctionsCollection( coll.elems.map( removeReorderAccessFunctions ):_* )
      case _ =>
        access.afs.filter({
          case ma: MapAccessFunction => true // just take MapAcess
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


