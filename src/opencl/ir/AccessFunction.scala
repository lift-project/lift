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
    println("AccessFunctions :+ ")
    new AccessFunctions(this.afs :+ af)
  }

  def ++(that: AccessFunctions): AccessFunctions = {
    new AccessFunctions(this.afs ++ that.afs)
  }
}

class AccessFunctionsCollection(val indexOfDataFlow: Int, val elems: AccessFunctions*) extends AccessFunctions(Array()) {
  override def :+(af: AccessFunction): AccessFunctionsCollection = {
    println("AccessFunctionsCollection :+ ")
    AccessFunctionsCollection( elems.map(_ :+ af):_* )
  }

  override def ++(that: AccessFunctions): AccessFunctionsCollection = {
    AccessFunctionsCollection( elems.map( _ ++ that ):_* )
  }
}

object AccessFunctionsCollection {
  def apply(indexOfDataFlow: Int, elems: AccessFunctions*): AccessFunctionsCollection = {
    new AccessFunctionsCollection(indexOfDataFlow, elems:_* )
  }

  def apply(elems: AccessFunctions*): AccessFunctionsCollection = {
    apply(0, elems:_*)
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

  private def determineInputAccessFun(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    call.f match {
      case r: AbstractPartRed =>
        if (call.args.length != 2) throw new NumberOfArgumentsException
        val initA  = addAccessFunctions(call.args(0), EmptyAccessFuntions)
        val inputA = addAccessFunctions(call.args(1), inputAccess)
        AccessFunctionsCollection(initA, inputA)

      case _ =>
        if (call.args.isEmpty)
          inputAccess
        else if (call.args.length == 1)
          addAccessFunctions(call.args(0), inputAccess)
        else { //more than one argument
          inputAccess match {
            // if the input is a collection unpack it
            case coll: AccessFunctionsCollection =>
              assert(call.args.length == coll.elems.length)
              val inAs = (call.args zip coll.elems).map({
                case (e, af) => addAccessFunctions(e, af)
              })

              AccessFunctionsCollection(inAs:_*)

            case EmptyAccessFuntions =>
              val inAs = call.args.map(addAccessFunctions(_, EmptyAccessFuntions))
              AccessFunctionsCollection(inAs:_*)

            case af: AccessFunctions =>
              val inAs = call.args.map(addAccessFunctions(_, inputAccess))
              assert(inAs.count(_ == inputAccess) == 1) // input access should only be used once!

              AccessFunctionsCollection(inAs.indexOf(inputAccess), inAs:_*)
          }
        }

    }
  }

  private def addAccessFunctionsFunCall(call: FunCall, inputAccess: AccessFunctions): AccessFunctions = {
    assert(call.inM != UnallocatedMemory && call.outM != UnallocatedMemory)

    /*
    println("call: " + call)
    inputAccess match {
      case coll: AccessFunctionsCollection =>
        println("\tafs: " + coll.elems.map(_.toString).reduce(_ + ", " + _))
      case af: AccessFunctions =>
        println("\taf: " + af.toString)
    }
    */

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
    /*
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
    */
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


    val outputAccess = inputAccess match {
      case coll: AccessFunctionsCollection => coll.elems(coll.indexOfDataFlow)
      case af: AccessFunctions => af
    }

    // output
    val outAccessFun = MapAccessFunction(call.loopVar, Cst(1), call.name)
    call.outAccess = updateAccessFunction(outputAccess, Type.length(call.outT).head) :+ outAccessFun

    // recurse
    addAccessFunctions(call.f.f.body, call.inAccess)

    outputAccess
  }

  private def addAccessFunctionsMapSeq(call: MapCall, inputAccess: AccessFunctions): AccessFunctions = {
    val m = call.f

    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(m.f.body.inT), "MapSeq")

    // input
    val inChunkSizes = Type.length(call.inT).reduce(_ * _) // this includes the vector size
    call.inAccess = updateAccessFunction(inputAccess, inChunkSizes) :+ innermostAccess

    val outputAccess = inputAccess match {
      case coll: AccessFunctionsCollection => coll.elems(coll.indexOfDataFlow)
      case af: AccessFunctions => af
    }

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    call.outAccess = updateAccessFunction(removeReorderAccessFunctions(outputAccess), outChunkSizes) :+ innermostAccess

    // recurse
    addAccessFunctions(call.f.f.body, call.inAccess)

    removeReorderAccessFunctions(outputAccess)
  }

  private def addAccessFunctionsReduce(call: ReduceCall, inputAccess: AccessFunctions): AccessFunctions = {

    val inT = call.inT match { case tt: TupleType => tt.elemsT(1) }

    val initA = inputAccess match { case coll: AccessFunctionsCollection => coll.elems(0) }
    val inA = inputAccess match { case coll: AccessFunctionsCollection => coll.elems(1) }

    val t = call.f.f.body.inT match { case tt: TupleType => tt.elemsT(1) } // type of the second argument
    val innermostAccess = AccessFunction( (_) => call.loopVar * Type.getVectorSize(t), "ReduceSeq")

    // input
    val inChunkSizes = Type.length(inT).reduce(_ * _) // this includes the vector size
    call.inAccess = AccessFunctionsCollection(initA, updateAccessFunction(inA, inChunkSizes) :+ innermostAccess)

    val outputAccess = inA match {
      case coll: AccessFunctionsCollection => coll.elems(coll.indexOfDataFlow)
      case af: AccessFunctions => af
    }

    // output
    val outChunkSizes = Type.length(call.outT).reduce(_ * _)  // this includes the vector size
    call.outAccess = updateAccessFunction(removeReorderAccessFunctions(outputAccess), outChunkSizes)

    // recurse
    addAccessFunctions(call.f.f.body, call.inAccess)

    removeReorderAccessFunctions(outputAccess)
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

    inputAccess :+ AccessFunction( (i:ArithExpr) => { i / n + s * (i % n) } , scope)
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


