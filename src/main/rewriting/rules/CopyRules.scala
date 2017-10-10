package rewriting.rules

import ir._
import ir.ast._
import opencl.ir.pattern._
import rewriting.utils.Utils

object CopyRules {

  val implementOneLevelOfId = Rule("Id() => ", {
    case FunCall(Id(), arg) =>
      generateId(arg.t) $ arg
  })

  val implementIdAsDeepCopy = Rule("Id() => ", {
    case FunCall(Id(), arg) =>
      generateCopy(arg.t) $ arg
  })

  val addId = Rule("f => f o Id()", {
    case FunCall(f, arg) =>
      f o Id() $ arg
  })

  val addIdRed = Rule("f => f o Id()", {
    case FunCall(f: AbstractPartRed, init, arg) =>
      f(init, Id() $ arg)
  })

  val addIdAfterReduce = Rule("f => Id() o f", {
    case call@FunCall(_: ReduceSeq, _*) =>
      FunCall(MapSeq(Id()), call)
  })

  val addIdValue = Rule("Value(...) => Id() $ Value(...)", {
    case v: Value =>
      FunCall(generateCopy(v.t), v)
  })

  def isId(expr: Expr): Boolean =
    expr match {
      case FunCall(Id(), _) => true
      case _ => false
    }

  val addIdForMapWrgParam = Rule("MapWrg(f) => MapWrg(f o Id())", {
    case FunCall(MapWrg(dim, f:Lambda1) , arg)
      if !f.body.contains( {case FunCall(MapWrg(_,_), _) =>})
      && !f.body.contains( {case FunCall(Id(), _) =>  })
      => MapWrg(dim, f o Id()) $ arg
  })

  val addIdForMapParam = Rule("Map(f) => Map(f o Id())", {
    case FunCall(Map(f:Lambda1) , arg)
      => Map(f o Id()) $ arg
  })

  private def generateCopy(t: Type): FunDecl = {
    t match {
      // TODO: How to properly distinguish between structs and several variables? This
      // TODO: could turn variables into a struct. Sometimes this also could be useful
      // Struct
      case TupleType(tt@_*) if tt.forall(t =>
        t.isInstanceOf[ScalarType] || t.isInstanceOf[VectorType]) =>
        UserFun("id" + Type.name(t), "x", "{ return x; }", t, t)

      // Several variables
      case TupleType(tt@_*) =>
        val newParam = Param()
        val argSequence = tt.zipWithIndex.map(p => generateCopy(p._1) $ Get(newParam, p._2))
        Lambda(Array(newParam), Tuple(argSequence:_*))

      case ArrayType(elemT) =>
        Map(generateCopy(elemT))
      case _ => generateId(t)
    }
  }

  private def generateId(t: Type): FunDecl = {
    t match {
      case TupleType(tt@_*) =>
        val newParam = Param()
        val argSequence = tt.zipWithIndex.map(p => Id() $ Get(newParam, p._2))
        Lambda(Array(newParam), Tuple(argSequence:_*))
      case ArrayType(_) =>
        Map(Id())
      case ScalarType(_, _) | VectorType(_, _) =>
        UserFun("id" + t, "x", "{ return x; }", t, t)
      case _ => throw TypeException(s"Can't generate id function for $t")
    }
  }
  val addIdBeforeMapLcl = Rule("MapLcl(f) => MapLcl(f) o Id()", {
    case call@FunCall(map:MapLcl, arg)
      if !isId(arg) && !call.context.inMapLcl.reduce(_ || _)
    =>
      map o Id() $ arg
  })

  val addIdBeforeMapSeq = Rule("MapSeq(f) => MapSeq(f) o Id()", {
    case call@FunCall(map: MapSeq, arg)
      if !isId(arg) && (call.context.inMapLcl.reduce(_ || _) || call.context.inMapGlb.reduce(_ || _))
    =>
      map o Id() $ arg
  })

  val addIdForCurrentValueInReduce = Rule("reduce", {
    case call@FunCall(ReduceSeq(l), _, _)
      if !Utils.visitFunCallChainWithState(false)(l.body, (e, b) => isId(e) || b) // TODO: Why?
      && !l.body.contains( {case FunCall(Id(), a) if a eq l.params(1)=>  })
    =>
      val params = l.params
      val body = l.body
      val newParam = Param()

      val paramForCurrent = params(1)

      val tempBody = Expr.replace(body, paramForCurrent, newParam)

      // Construct the FunCall directly to avoid inlining
      val newBody =
        FunCall(Lambda(Array(newParam), tempBody),  FunCall(Id(), paramForCurrent))

      Expr.replace(call, body, newBody)
  })

  val addCopy = Rule("f => f o Id()", {
    case FunCall(f, arg) =>
      f o generateCopy(arg.t) $ arg
  })

  val tupleToStruct = Rule("tupleToStruct", {
    case call@FunCall(Tuple(_), _*)
      if {
        var containsArray = false

        Type.visit(call.t, _ => Unit,
          t => if (t.isInstanceOf[ArrayType]) containsArray = true)

        !containsArray
      }
    =>
      val copyFun = generateCopy(call.t)
      copyFun $ call
  })
}
