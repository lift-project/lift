package opencl.generator

import apart.arithmetic._
import arithmetic.TypeVar
import ir._
import ir.ast._
import ir.view.AccessVar
import opencl.ir._
import opencl.ir.ast._


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
    println(s"/* $comment */")
  }

  private def printSpace() {
    sb ++= " " * tab
  }

  def println(s: String = "") {
    print(s)
    sb +=  '\n'
    newline = true
  }

  def print(s: String) {
    if (newline)
      printSpace()
    sb ++= s
    //System.out.println(sb.toString())
    newline = false
  }

  def printVarDecl(t: Type, v: Var, init: String) {
    println(toOpenCL(t)+" "+toOpenCL(v)+" = "+init + ";")
  }

  def printVarDecl(t: Type, v: Var) {
    println(toOpenCL(t)+" "+toOpenCL(v)+ ";")
  }

  def printVarDecl(mem: TypedOpenCLMemory): Unit = {
    if (mem.mem.addressSpace != PrivateMemory) {
      val baseType = Type.getBaseType(mem.t)
      println(mem.mem.addressSpace + " " + toOpenCL(baseType) + " " +
        toOpenCL(mem.mem.variable) + "[" + toOpenCL(mem.mem.size /^ Type.getSize(baseType)) + "];")
    } else {
      if (mem.t.isInstanceOf[ArrayType]) {
        val baseType = Type.getBaseType(mem.t)
        val length = (mem.mem.size /^ Type.getSize(baseType)).eval
        for (i <- 0 until length)
          println(toOpenCL(baseType) + " " + toOpenCL(mem.mem.variable) +
            "_" + toOpenCL(i) + ";")
      } else {
        printVarDecl(Type.getValueType(mem.t), mem.mem.variable)
      }
    }
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
      case ScalarType(_,_) | VectorType(_,_) =>
        toOpenCL(Type.devectorize(mem.t)) + " " + toOpenCL(mem.mem.variable)
      case ArrayType(_,_) =>
        // Const restricted pointers to read-only global memory. See issue #2.
        val const = if (mem.mem.readOnly) "const " else ""
        val restrict = if (mem.mem.readOnly) "restrict " else ""
        const + mem.mem.addressSpace + " " + toOpenCL(Type.devectorize(mem.t)) + " " + restrict + toOpenCL(mem.mem.variable)
    }
  }

  def printAsParameterDecl(mems: Array[TypedOpenCLMemory]) {
    print(mems.map( mem => toParameterDecl(mem) ).mkString(", "))
  }

  def generateFunCall(expr: Expr, args: String*) {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun => generateFunCall(uf, args:_*)
        //case vf: Vectorize => generateFunCall(UserFun.vectorize(vf.f.asInstanceOf[UserFun], vf.n), args:_*)
        case l: Lambda => generateFunCall(l.body, args:_*)
        case _ => throw new NotImplementedError()
      }
      case _ => throw new NotImplementedError()
    }
  }

  def generateFunCall(f: UserFun, args: String*) {
    print(f.name+"(")
    if (args.nonEmpty)
      print(args.mkString(","))
    print(")")
  }

  def toOpenCL(t: Type, seenArray: Boolean = false) : String = {
    t match {
      case ArrayType(elemT, _) =>
        val s = toOpenCL(elemT, seenArray=true)
        if (!seenArray) s + "*" else s
//      case MatrixType(elemT, _, _) =>
//        val s = toOpenCL(elemT, seenArray=true)
//        if (!seenArray) s + "*" else s
      case VectorType(elemT, len) => toOpenCL(elemT, seenArray) + toOpenCL(len)
      case ScalarType(name, _) => name
      case tt: TupleType => Type.name(tt)
      case UndefType => "void"
    }
  }

  def toOpenCL(e: ArithExpr) : String = {
    val me = e
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
      case of: OclFunction => of.toOCLString
      case tv : TypeVar => "tv_"+tv.id
      case ai: AccessVar => ai.array + "[" + toOpenCL(ai.idx) + "]"
      case v: Var => "v_"+v.name+"_"+v.id
      case IntDiv(n, d) => "(" + toOpenCL(n) + " / " + toOpenCL(d) + ")"
      case gc: GroupCall =>
        val outerAe = gc.outerAe
        val innerAe = gc.outerAe
        val len = gc.len
        "groupComp" + gc.group.id + "(" + toOpenCL(outerAe) + ", " +
          toOpenCL(innerAe) + ", " + toOpenCL(len) + ")"
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
        (tt.elemsT zip names).map( {case (t,n) => toOpenCL( (t, n) ) }).mkString(", ")
      case _ => throw new NotPrintableExpression( param.toString() )
    }
  }

  def toOpenCL(uf: UserFun) : String = {
    val typedefs = uf.tupleTypes.map(createTypedef).fold("")(_+_)
    val params = toOpenCL( (uf.inT, uf.paramName) )

    typedefs +
      toOpenCL(uf.outT) + " " + uf.name + "(" + params + ") {" +
      createTupleAlias(uf.tupleTypes) +
      uf.body + "}"
  }

  def toOpenCL(group: Group) : String = {
    group.paramType match {
      case ArrayType(t, len) =>
        val lenVar = Var("length")
        val newIdx = Var("newIdx")
        val newIdxStr = toOpenCL(newIdx)

        s"""
           |int groupComp${group.id}(int j, int i, int ${toOpenCL(lenVar)}){
           |  // Compute new index
           |  int relIndices[] = {${group.relIndices.deep.mkString(", ")}};
           |  int $newIdxStr = j + relIndices[i];
           |
           |  // Boundary check
           |  if ($newIdxStr < 0) {
           |    return ${toOpenCL(group.negOutOfBoundsF(newIdx, lenVar))};
           |  } else if ($newIdxStr >= ${toOpenCL(lenVar)}) {
           |    return ${toOpenCL(group.posOutOfBoundsF(newIdx - lenVar + 1, lenVar))};
           |  } else {
           |    return $newIdxStr;
           |  }
           |}
         """.stripMargin
      case _ => throw new IllegalArgumentException
    }
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
      tts.zipWithIndex.map({case (tt, i) => "typedef " + Type.name(tt) + s" Tuple$i;"}).mkString(" ")
    }
  }

  def generateBarrier(mem : Memory) {
    mem match {
      case m : OpenCLMemory => generateBarrier(m)
      case _ =>
    }
  }

  def generateBarrier(mem : OpenCLMemory) = println (mem.addressSpace match {
    case GlobalMemory => "barrier(CLK_GLOBAL_MEM_FENCE);"
    case LocalMemory => "barrier(CLK_LOCAL_MEM_FENCE);"
    case x => "barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);"
  })


  def generateLoop(indexVar: Var, printBody: () => Unit, iterationCount: ArithExpr = ?) {
    val range = indexVar.range.asInstanceOf[RangeAdd]

    val init = range.start
    val cond = range.stop
    val update = range.step

    iterationCount match {
      case Cst(0) =>

      case Cst(1) =>
        // exactly one iteration
        openCB ()
        println ("int " + toOpenCL (indexVar) + " = " + toOpenCL (init) + ";")
        printBody ()
        closeCB ()

      case IntDiv (Cst(1), x) if x.getClass == ?.getClass =>
        // one or less iteration
        openCB ()
        println ("int " + toOpenCL (indexVar) + " = " + toOpenCL (init) + ";")
        print ("if (" + toOpenCL (indexVar) + " < (" + toOpenCL (cond) + ")) ")
        openCB ()
        printBody ()
        closeCB ()
        closeCB ()

      case _ =>
        // as the default print of the default loop
        print ("for (int " + toOpenCL (indexVar) + " = " + toOpenCL (init) + "; " +
          toOpenCL (indexVar) + " < " + toOpenCL (cond) + "; " +
          toOpenCL (indexVar) + " += " + toOpenCL (update) + ") ")
        openCB ()
        printBody ()
        closeCB ()
    }
  }

}
