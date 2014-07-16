package opencl.generator

import ir._
import opencl.ir.{LocalMemory, GlobalMemory, OpenCLMemory}


class OpenCLPrinter {

  private var tab = 0
  private var newline = true

  private val sb = new StringBuilder

  def code = sb.toString()

  def indent() {
    tab += 1
  }

  def undent() {
    tab -= 1
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

  def printAsParameterDecl(input: Input) {
    val t = input.expectedOutT
    print(t match {
      case TupleType(_) => throw new Exception // TODO: handle this ..., create multiple variables
      case _ => "global " + toOpenCL(t) + " " + toOpenCL(input.variable)
    })
  }

  private def toParameterDecl(mem: OpenCLMemory) : String = {
    mem.addressSpace + " " + toOpenCL(mem.t) + " " + toOpenCL(mem.variable)
  }

  def printAsParameterDecl(mems: Array[OpenCLMemory]) {
    print(mems.map(m => toParameterDecl(OpenCLMemory.asOpenCLMemory(m))
    ).reduce(separateByComma))
  }

  def generateFunCall(f: UserFun, args: String*) {
    print(f.name)
    if (args.length > 0)
      print("("+args.reduceLeft((result, a) => result + "," + a)+")")
  }

  def separateByComma(lhs: Any, rhs: Any) = {
    lhs + ", " + rhs
  }

  def toOpenCL(t: Type, seenArray: Boolean = false) : String = {
    t match {
      case ArrayType(elemT, _) =>
        val s = toOpenCL(elemT, true)
        if (!seenArray) s + "*" else s
      case VectorType(elemT, len) => toOpenCL(elemT, seenArray) + toOpenCL(len)
      case ScalarType(name, _) => name
      case TupleType(_) => throw new Exception // don't know how to print a tuple in opencl ...
      case UndefType => "void"
    }
  }

  def toOpenCL(e: Expr) : String = {
    val me = if(Debug()) { e } else { ExprSimplifier.simplify(e) }
    me match {
      case Cst(c) => c.toString
      case Pow(b, ex) => "pow(" + toOpenCL(b) + ", " + toOpenCL(ex) + ")"
      case Prod(es) => "(" + es.foldLeft("1")( (s: String, e: Expr) => {
        s + (e match {
          case Pow(b, Cst(-1)) => " / (" + toOpenCL(b) + ")"
          case _ => " * " + toOpenCL(e)
        })
      } ).drop(4) /* drop "1 * " */ + ")"
      case Sum(es) => "(" + es.map(toOpenCL).reduce( _ + " + " + _  ) + ")"
      case of: OclFunction => of.toOCLString
      case tv : TypeVar => "t_"+tv.id
      case Var(n, _) => "v_"+n
      case _ => throw new NotPrintableExpression(me.toString)
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
    }
    if (mem.addressSpace == LocalMemory) {
      println("barrier(CLK_LOCAL_MEM_FENCE);")
    }
    println("barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE);")
  }


  def generateLoop(indexVar: Var, range: RangeAdd, printBody: (() => Unit)) {

    val init = ExprSimplifier.simplify(range.start)
    val cond = ExprSimplifier.simplify(range.stop)
    val update = ExprSimplifier.simplify(range.step)

    // eval expression. if sucessfull return true and the value, otherwise return false
    def evalExpr = (e: Expr) => { try { (true, e.eval()) } catch { case _ : Throwable => (false, 0) } }

    // try to directly evaluate
    val (initIsEvaluated, initEvaluated) = evalExpr(init)
    val (condIsEvaluated, condEvaluated) = evalExpr(cond)
    val (updateIsEvaluated, updateEvaluated) = evalExpr(update)

    // if all three can be evaluated ...
    if (initIsEvaluated && condIsEvaluated && updateIsEvaluated)
      // .. and the condition is less or equal than init + update then exactly one iteration is necessary
      if (condEvaluated <= ( initEvaluated + updateEvaluated )) {
        openCB()
        println("int " + toOpenCL(indexVar) + " = " + toOpenCL(init) + ";")
        printBody()
        closeCB()
        return
      }

    // if condition and update can be evaluated ...
    if (condIsEvaluated && updateIsEvaluated)
      // ... and the condition is less than the update then at most one iteration is necessary
      if (condEvaluated <= updateEvaluated) {
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
    println ("for (int " + toOpenCL(indexVar) + " = " + toOpenCL(init)  + "; " +
      toOpenCL(indexVar) + " < " + toOpenCL(cond)  + "; " +
      toOpenCL(indexVar) + " += " + toOpenCL(update) + ")")
    openCB()
    printBody()
    closeCB()
  }

}
