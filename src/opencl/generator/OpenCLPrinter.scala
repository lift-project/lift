package opencl.generator

import ir._
import opencl.ir._


class OpenCLPrinter {

  private var tab = 0
  private var newline = true

  private val sb = new StringBuilder

  def code = sb.toString()

  def indent() {
    tab += 2
  }

  def undent() {
    tab -= 2
  }

  def openCB() = {
    println("{")
    indent()
  }

  def closeCB() = {
    undent()
    println("}")
  }

  def commln(comment: String) {
    println("/* "+comment+" */")
  }

  private def printSpace() {
    1 to tab foreach { _ => sb ++= " "}
  }

  def println(s: String = "") {
    print(s)
    sb ++=  "\n"
    newline = true
  }

  def print(s: String) {
    if (newline)
      printSpace()
    sb ++= s
    newline = false
  }

  def printVarDecl(t: Type, v: Var, init: String) {
    print(toOpenCL(t)+" "+toOpenCL(v)+" = "+init)
  }

  def printVarDecl(t: Type, v: TypeVar, init: String) {
    print(toOpenCL(t)+" "+toOpenCL(v)+" = "+init)
  }
/*
  def printAsParameterDecl(input: Input) {
    val t = input.expectedOutT
    print(t match {
      case TupleType(_) => throw new Exception // TODO: handle this ..., create multiple variables
      case _ => "global " + toOpenCL(t) + " " + toOpenCL(input.variable)
    })
  }
*/
  private def toParameterDecl(mem: TypedOpenCLMemory) : String = {
    mem.t match {
      case ScalarType(_,_) | VectorType(_,_) => toOpenCL(Type.devectorize(mem.t)) + " " + toOpenCL(mem.mem.variable)
      case ArrayType(_,_) => mem.mem.addressSpace + " " + toOpenCL(Type.devectorize(mem.t)) + " " + toOpenCL(mem.mem.variable)
    }
  }

  def printAsParameterDecl(mems: Array[TypedOpenCLMemory]) {
    print(mems.map( mem => toParameterDecl(mem) ).reduce(separateByComma))
  }

  def generateFunCall(expr: Expr, args: String*) {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFunDef => generateFunCall(uf, args:_*)
        //case vf: Vectorize => generateFunCall(UserFun.vectorize(vf.f.asInstanceOf[UserFun], vf.n), args:_*)
        case l: Lambda => generateFunCall(l.body, args:_*)
        case _ => throw new NotImplementedError()
      }
      case _ => throw new NotImplementedError()
    }
  }

  def generateFunCall(f: UserFunDef, args: String*) {
    print(f.name+"(")
    if (args.length > 0)
      print(args.reduceLeft((result, a) => result + "," + a))
    print(")")
  }

  def separateByComma(lhs: Any, rhs: Any) = {
    lhs + ", " + rhs
  }

  def toOpenCL(t: Type, seenArray: Boolean = false) : String = {
    t match {
      case ArrayType(elemT, _) =>
        val s = toOpenCL(elemT, seenArray=true)
        if (!seenArray) s + "*" else s
      case VectorType(elemT, len) => toOpenCL(elemT, seenArray) + toOpenCL(len)
      case ScalarType(name, _) => name
      case tt: TupleType => Type.name(tt)
      case UndefType => "void"
    }
  }

  def toOpenCL(e: ArithExpr) : String = {
    val me = if(Debug()) { e } else { ExprSimplifier.simplify(e) }
    me match {
      case Cst(c) => c.toString
      case Pow(b, ex) => "(int)pow((float)" + toOpenCL(b) + ", " + toOpenCL(ex) + ")"
      case Log(b, x) => "(int)log"+b+"((float)"+toOpenCL(x)+")"
      case Prod(es) => "(" + es.foldLeft("1")( (s: String, e: ArithExpr) => {
        s + (e match {
          case Pow(b, Cst(-1)) => " / (" + toOpenCL(b) + ")"
          case _ => " * " + toOpenCL(e)
        })
      } ).drop(4) /* drop "1 * " */ + ")"
      case Sum(es) => "(" + es.map(toOpenCL).reduce( _ + " + " + _  ) + ")"
      case Mod(a,n) => "(" + toOpenCL(a) + " % " + toOpenCL(n) + ")"
      case And(lhs, rhs) => "(" + toOpenCL(lhs) + " & " + toOpenCL(rhs) + ")"
      case of: OclFunction => of.toOCLString
      case tv : TypeVar => "tv_"+tv.id
      case v: Var => "v_"+v.name+"_"+v.id
      case _ => throw new NotPrintableExpression(me.toString)
    }
  }

  def toOpenCL(param: (Type, Any)): String = {
    param match {
      case (st: ScalarType, name: String) => toOpenCL(st) + " " + name
      case (vt: VectorType, name: String) => toOpenCL(vt) + " " + name
      case (tt: TupleType, name: String) => toOpenCL(tt) + " " + name
      case (tt: TupleType, names: Array[Any]) =>
        assert(tt.elemsT.length == names.length)
        (tt.elemsT zip names).map( {case (t,n) => toOpenCL( (t, n) ) }).reduce(separateByComma)
      case _ => throw new NotPrintableExpression( param.toString() )
    }
  }

  def toOpenCL(uf: UserFunDef) : String = {
    val typedefs = uf.unexpandedTupleTypes.map(createTypedef).fold("")(_+_)
    val params = toOpenCL( (uf.inT, uf.paramNames) )

    typedefs +
      toOpenCL(uf.outT) + " " + uf.name + "(" + params + ") {" +
      createTupleAlias(uf.unexpandedTupleTypes) +
      uf.body + "}"
  }

  def createTypedef(t: Type): String = {
    t match {
      case tt: TupleType =>
        val name = Type.name(tt)
        val fields = tt.elemsT.zipWithIndex.map({case (ty,i) => Type.name(ty)+" _"+i})
        s"""#ifndef ${name}_DEFINED
           |#define ${name}_DEFINED
           |typedef struct {
           |  ${fields.reduce(_+";\n  "+_)};
           |} $name;
           |#endif
           |""".stripMargin
      case _ => ""
    }
  }

  def createTupleAlias(tts: Seq[TupleType]): String = {
    if (tts.isEmpty) return ""
    if (tts.size == 1) "typedef " + Type.name(tts.head) + " Tuple; "
    else {
      // TODO: think about this one ...
      tts.zipWithIndex.map({case (tt, i) => "typedef " + Type.name(tt) + s" Tuple$i;"}).reduce(_+" "+_)
    }
  }


  def generateBarrier(mem : Memory) {
    mem match {
      case m : OpenCLMemory => generateBarrier(m)
      case _ =>
    }
  }

  def generateBarrier(mem : OpenCLMemory) {
    if (mem.addressSpace == GlobalMemory) {
      println("barrier(CLK_GLOBAL_MEM_FENCE);")
    } else
    if (mem.addressSpace == LocalMemory) {
      println("barrier(CLK_LOCAL_MEM_FENCE);")
    } else {
      println("barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE);")
    }
  }


  def generateLoop(indexVar: Var, range: RangeAdd, printBody: (() => Unit)) {
    indexVar.range = range

    val init = ExprSimplifier.simplify(range.start)
    val cond = ExprSimplifier.simplify(range.stop)
    val update = ExprSimplifier.simplify(range.step)

    // eval expression. if sucessful return true and the value, otherwise return false
    def evalExpr = (e: ArithExpr) => {try { (true, e.evalAtMax())} catch { case _ : Throwable => (false, 0) } }

    // try to directly evaluate
    val (initIsEvaluated, initEvaluated) = evalExpr(init)
    val (condIsEvaluated, condEvaluated) = evalExpr(cond)
    val (updateIsEvaluated, updateEvaluated) = evalExpr(update)

    // TODO evaluate symbolically with a comparison operator (add support for <,<=,==,>=,> in Expr)

    if (initIsEvaluated && condIsEvaluated) {
      if (condEvaluated <= initEvaluated)
        // nothing to do
        return
    }

    if (initIsEvaluated && condIsEvaluated && updateIsEvaluated) {
      assert (condEvaluated > initEvaluated)
      if (condEvaluated <= (initEvaluated + updateEvaluated)) {
        // exactly one iteration
        openCB()
        println("int " + toOpenCL(indexVar) + " = " + toOpenCL(init) + ";")
        printBody()
        closeCB()
        return
      }
    }

    if (condIsEvaluated && updateIsEvaluated)
      if (condEvaluated <= updateEvaluated) {
        // one or less iteration
        openCB()
        println("int " + toOpenCL(indexVar) + " = " + toOpenCL(init) + ";")
        print("if (" + toOpenCL(indexVar) + " < (" + toOpenCL(cond) + ")) ")
        openCB()
        printBody()
        closeCB()
        closeCB()
        return
      }


    // as the default print of the default loop
    print ("for (int " + toOpenCL(indexVar) + " = " + toOpenCL(init)  + "; " +
      toOpenCL(indexVar) + " < " + toOpenCL(cond)  + "; " +
      toOpenCL(indexVar) + " += " + toOpenCL(update) + ") ")
    openCB()
    printBody()
    closeCB()
  }

}
