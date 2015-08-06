package opencl.generator

import apart.arithmetic._
import arithmetic.TypeVar
import ir.ast._
import ir._
import ir.view.AccessVar
import opencl.generator.OpenCLAST._
import opencl.ir._
import opencl.ir.ast.GroupCall

/** The codegen walks the AST emitted by the [[OpenCLGenerator]] and generates
  * standalone OpenCL-C code.
  */
object OpenCLCodeGen {
  /** Output stream for current AST */
  var sb: StringBuilder = new StringBuilder

  /** Stream interpolation helper */
  implicit class OCLInterpolation(val sc: StringContext) extends AnyVal {
    def ocl(args: Any*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while(strings.hasNext) {
        val str: String = expressions.next match {
          case a: ArithExpr => toOpenCL(a)
          case x => x.toString
        }
        buf append str
        buf append strings.next
      }
      buf.toString
    }
  }

  def print(s: String): Unit = {
    sb ++= s
  }

  def println(s: String): Unit = {
    sb ++= line{s}
  }

  /** Current indentation (depth of scope) */
  var indent = 0

  /** Create a block between braces. */
  def block(code: => Unit): Unit = {
    indent += 1
    sb ++= line {"{"}
    code
    indent -= 1
    sb ++= line() + line {"}"}
  }

  /** Print the given string an create an indented new line */
  def line(str: String = "") = str + "\n" + tab()
  /** Insert the correct indentation */
  def tab() = s"  " * indent

  /**
   * Main visit method. Print the current node and recurse.
   * @param node The current node to emit code for.
   * @param statement A flag set if the statement to be emitted is a full statement (ie ending with ';')
   */
  def visit(node: OclAstNode, statement: Boolean = true): Unit = node match {
    case b: Block =>
      if(b.global) b.content.foreach(visit(_))
      else block { b.content.foreach(visit(_)) }

    case f: Function => generate(f)
    case i: Inline => sb ++= i.code
    case c: Comment => println(s"/* ${c.content} */")
    case v: VarDecl => generateVarDecl(v)
    case v: VarRef => generate(v)
    case p: ParamDecl => generate(p)
    case t: TypeDef => generateTypedef(t)
    case b: Barrier => generate(b)
    case l: Loop =>    generate(l)
    case e: Expression => print(toOpenCL(e.content))
    case a: Assignment => generate(a)
    case f: FunctionCall => generate(f)

    case x => print(s"/* UNKNOWN: ${x.getClass.getSimpleName} */")
  }

  /**
   * Entry point for printing an AST.
   * @param node The root of the AST (the global scope block).
   * @return A string representation of the AST as OpenCL-C code.
   */
  def apply(node: OclAstNode): String = {
    sb = new StringBuilder()
    indent = 0
    visit(node)
    sb.toString()
  }












  def generate(f: FunctionCall) = {
    print(f.name + "(")
    f.params.foreach(x => {
      visit(x)
      if(x != f.params.last) print(", ")
    })
    print(")")
  }

  def generate(v: VarRef): Unit = {
    print(v.name)
    if(v.offset != null) {
      print("[")
      visit(v.offset)
      print("]")
    }
  }

  def generate(f: Function): Unit = {
    if(f.kernel) sb ++= "kernel void"
    else sb ++= f.ret.toString
    sb ++= s" ${f.name}("
    f.params.foreach(x => {
      visit(x, statement = false)
      if(x != f.params.last) sb ++= ", "
    })
    sb ++= ")"
    visit(f.body)
    sb ++= line("")
  }

  def generate(a: Assignment) = {
    visit(a.to)
    print(" = ")
    visit(a.value)
    println(";")
  }

  def generate(p: ParamDecl) = p.t match {
    case ScalarType(_,_) | VectorType(_,_) =>
      print(toOpenCL(Type.devectorize(p.t)) + " " + p.name)

    case ArrayType(_,_) =>
      // Const restricted pointers to read-only global memory. See issue #2.
      val (const, restrict) = if (p.const) ("const ", "restrict ") else ("","")
      print(const + p.addressSpace + " " + toOpenCL(Type.devectorize(p.t)) + " " + restrict + p.name)
  }

  def generateVarDecl(v: VarDecl) {
    if (v.addressSpace != PrivateMemory && v.addressSpace != UndefAddressSpace) {
      val baseType = Type.getBaseType(v.t)
      println(s"${v.addressSpace} ${toOpenCL(baseType)} ${v.name}[${v.length}];")
    }
    else {
      v.t match {
        case a: ArrayType =>
          for (i <- 0 until v.length)
            println(toOpenCL(Type.getBaseType(v.t)) + " " + v.name + "_" + toOpenCL(i) + ";")

        case x =>
          print(toOpenCL(v.t)+" "+v.name)
          if(v.init != null) {
            print(s" = ")
            visit(v.init)
          }
          println(";")
      }
    }
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
    val typedefs = uf.tupleTypes.map(generateTypedef).fold("")(_+_)
    val params = toOpenCL( (uf.inT, uf.paramName) )

    typedefs +
      toOpenCL(uf.outT) + " " + uf.name + "(" + params + ") {" +
      createTupleAlias(uf.tupleTypes) +
      uf.body + "}"
  }

  def generateTypedef(t: Type): String = {
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

  def generateTypedef(t: TypeDef): String = {
    t.t match {
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

  /**
   * Generate a barrier for the given address space scope.
   * If the scope is not defined as global or local, the barrier assumes both.
   * @param b A [[Barrier]] node.
   */
  def generate(b: Barrier) = println (b.mem.addressSpace match {
    case GlobalMemory => "barrier(CLK_GLOBAL_MEM_FENCE);"
    case LocalMemory => "barrier(CLK_LOCAL_MEM_FENCE);"
    case x => "barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);"
  })


  /**
   * Generate a loop. The following optimizations are applied:
   *  - dead loops do not emit any code
   *  - loops with a trip count of 1 are emitted as if statements
   *  - anything else is a for-loop
   * @param l a [[Loop]] node.
   */
  def generate(l: Loop) {
    val range = l.indexVar.range.asInstanceOf[RangeAdd]

    val init = range.start
    val cond = range.stop
    val update = range.step

    // as the default print of the default loop
    print ("for (int " + toOpenCL (l.indexVar) + " = " + toOpenCL (init) + "; " +
      toOpenCL (l.indexVar) + " < " + toOpenCL (cond) + "; " +
      toOpenCL (l.indexVar) + " += " + toOpenCL (update) + ") ")
    block {
      visit(l.body)
    }
  }
}
