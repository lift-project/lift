package opencl.generator

import apart.arithmetic._
import arithmetic.TypeVar
import ir._
import ir.view.AccessVar
import opencl.generator.OpenCLAST._
import opencl.ir._
import opencl.ir.ast.GroupCall

object OpenCLCodeGen {
  def apply() = new OpenCLCodeGen
}

/** The codegen walks the AST emitted by the [[OpenCLGenerator]] and generates
  * standalone OpenCL-C code.
  */
class OpenCLCodeGen {
  /**
   * Entry point for printing an AST.
   * @param node The root of the AST (the global scope block).
   * @return A string representation of the AST as OpenCL-C code.
   */
  def apply(node: OclAstNode): String = {
    indent = 0
    print(node)
    sb.toString()
  }

  def print(t: Type, seenArray: Boolean = false) : String = {
    t match {
      case ArrayType(elemT, _) =>
        val s = print(elemT, seenArray=true)
        if (!seenArray) s + "*" else s
      case VectorType(elemT, len) => print(elemT, seenArray) + print(len)
      case ScalarType(name, _) => name
      case tt: TupleType => Type.name(tt)
      case NoType => "void"
      case _ => throw new NotPrintableExpression(t.toString)
    }
  }

  def print(e: ArithExpr) : String = {
    val me = e
    me match {
      case Cst(c) => c.toString
      case Pow(b, ex) =>
        "(int)pow((float)" + print(b) + ", " + print(ex) + ")"
      case Log(b, x) => "(int)log"+b+"((float)"+print(x)+")"
      case Prod(es) => "(" + es.foldLeft("1")( (s: String, e: ArithExpr) => {
        s + (e match {
          case Pow(b, Cst(-1)) => " / (" + print(b) + ")"
          case _ => " * " + print(e)
        })
      } ).drop(4) + ")" // drop(4) removes the initial "1 * "
      case Sum(es) => "(" + es.map(print).reduce( _ + " + " + _  ) + ")"
      case Mod(a,n) => "(" + print(a) + " % " + print(n) + ")"
      case of: OclFunction => of.toOCLString
      case ai: AccessVar => ai.array + "[" + print(ai.idx) + "]"
      case v: Var => v.toString
      case IntDiv(n, d) => "(" + print(n) + " / " + print(d) + ")"
      case gc: GroupCall =>
        val outerAe = gc.outerAe
        val innerAe = gc.innerAe
        "groupComp" + gc.group.id + "(" + print(outerAe) + ", " +
        print(innerAe) + ")"
      case i: IfThenElse =>
        s"( (${print(i.test.lhs)} ${i.test.op} ${print(i.test.rhs)}) ? " +
        s"${print(i.t)} : ${print(i.e)} )"
      case _ => throw new NotPrintableExpression(me.toString)
    }
  }

  // private implementation

  /** Output stream for current AST */
  private val sb: StringBuilder = new StringBuilder

  private def print(s: String): Unit = {
    sb ++= s
  }

  private def println(s: String): Unit = {
    sb ++= s + "\n" + tab()
  }

  /** Current indentation (depth of scope) */
  private var indent: Int = 0

  /** Create a block between braces. */
  private def printBlock(code: => Unit): Unit = {
    indent += 1
    println("{")
    code
    indent -= 1
    moveCursorBack(tabSize)
    println("}")
  }

  /** Print the given string an create an indented new line */
  private val tabSize = 2

  /** Insert the correct indentation */
  private def tab() = {
    lazy val whiteSpace: String = " " * tabSize
    whiteSpace * indent
  }

  /** Move cursor back by given size. Used to fix indentation */
  private def moveCursorBack(size: Int) = {
    for (_ <- 1 to size) {
      if (sb.last.isWhitespace) { sb.deleteCharAt(sb.size - 1) }
    }
  }

  /**
   * Main print method. Print the current node and recurse.
   * @param node The current node to emit code for.
   */
  private def print(node: OclAstNode): Unit = node match {
    case b: Block =>
      if(b.global) b.content.foreach(print)
      else printBlock { b.content.foreach(print) }

    case f: Function      => print(f)
    case i: OpenCLCode    => sb ++= i.code
    case c: Comment       => println(s"/* ${c.content} */")
    case v: VarDecl       => print(v)
    case v: VarRef        => print(v)
    case p: ParamDecl     => print(p)
    case b: Barrier       => print(b)
    case l: Loop          => print(l)
    case e: Expression    => print(print(e.content))
    case a: Assignment    => print(a)
    case f: FunctionCall  => print(f)
    case l: Load          => print(l)
    case s: Store         => print(s)
    case t: TypeDef       => print(t)
    case a: TupleAlias    => print(a)
    case c: Cast          => print(c)
    case e: Extension     => print(e)

    case x => print(s"/* UNKNOWN: ${x.getClass.getSimpleName} */")
  }

  private def print(c: Cast): Unit = {
    print(s"(${c.t})")
    print(c.v)
  }

  private def print(t: TypeDef): Unit = t.t match {
    case tt: TupleType =>
      val name = Type.name(tt)
      val fields = tt.elemsT.zipWithIndex.map({case (ty,i) => Type.name(ty)+" _"+i})
      print(s"""#ifndef ${name}_DEFINED
        |#define ${name}_DEFINED
        |typedef struct {
        |  ${fields.reduce(_+";\n  "+_)};
        |} $name;
        |#endif
        |""".stripMargin)
    case _ =>
  }

  private def print(e: Extension): Unit = {
    println(s"#pragma OPENCL EXTENSION ${e.content} : enable")
  }

  private def print(alias: TupleAlias): Unit = alias.t match {
    case tt: TupleType =>
      println(s"typedef ${Type.name(tt)} ${alias.name};")
    case _ =>
  }

  private def print(l: Load): Unit = {
    print(s"vload${l.t.len}(")
    print(l.offset)
    print(",")
    print(l.v)
    print(")")
  }

  private def print(s: Store): Unit = {
    print(s"vstore${s.t.len}(")
    print(s.value)
    print(",")
    print(s.offset)
    print(",")
    print(s.v)
    println(");")
  }

  private def print(f: FunctionCall): Unit = {
    print(f.name + "(")
    f.args.foreach(x => {
      print(x)
      if(x != f.args.last) print(", ")
    })
    print(")")
  }

  private def print(v: VarRef): Unit = {
    print(v.name)
    if(v.index != null) {
      print("[")
      print(v.index)
      print("]")
    }
  }

  private def print(f: Function): Unit = {
    if(f.kernel) sb ++= "kernel void"
    else sb ++= print(f.ret)
    sb ++= s" ${f.name}("
    f.params.foreach(x => {
      print(x)
      if(x != f.params.last) sb ++= ", "
    })
    sb ++= ")"
    if(f.kernel)
      sb ++= "{ \n" +
        "#ifndef WORKGROUP_GUARD\n" +
        "#define WORKGROUP_GUARD\n" + 
        "#endif\n" +
        "WORKGROUP_GUARD\n"
    print(f.body)
    if(f.kernel)
      println("}")
  }

  private def print(a: Assignment): Unit = {
    print(a.to)
    print(" = ")
    print(a.value)
    println(";")
  }

  private def print(p: ParamDecl): Unit = p.t match {
    case ArrayType(_,_) =>
      // Const restricted pointers to read-only global memory. See issue #2.
      val (const, restrict) = if (p.const) ("const ", "restrict ") else ("","")
      print(const + p.addressSpace + " " + print(Type.devectorize(p.t)) +
            " " + restrict + p.name)

    case x =>
      print(print(p.t) + " " + p.name)
  }

  private def print(v: VarDecl): Unit = v.t match {
    case a: ArrayType =>
      v.addressSpace match {
        case PrivateMemory =>
          for (i <- 0 until v.length)
            println(print(Type.getBaseType(v.t)) + " " + v.name + "_" +
                    print(i) + ";")

        case LocalMemory if v.length != 0 =>
          val baseType = Type.getBaseType(v.t)
          println(s"${v.addressSpace} ${print(baseType)} " +
                  s"${v.name}[${v.length}];")

        case x =>
          val baseType = Type.getBaseType(v.t)
          print(s"${v.addressSpace} ${print(baseType)} *${v.name}")
          if(v.init != null) {
            print(s" = ")
            print(v.init)
          }
          println(";")
      }

    case x =>
      if(v.addressSpace == LocalMemory)
        print(v.addressSpace + " ")
      print(s"${print(v.t)} ${v.name}")
      if(v.init != null) {
        print(s" = ")
        print(v.init)
      }
      println(";")
  }

  /**
   * Generate a barrier for the given address space scope.
   * If the scope is not defined as global or local, the barrier assumes both.
   * @param b A [[Barrier]] node.
   */
  private def print(b: Barrier) = println (b.mem.addressSpace match {
    case GlobalMemory => "barrier(CLK_GLOBAL_MEM_FENCE);"
    case LocalMemory => "barrier(CLK_LOCAL_MEM_FENCE);"
    case _ => "barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);"
  })


  /**
   * Generate a loop. The following optimizations are applied:
   *  - dead loops do not emit any code
   *  - loops with a trip count of 1 are emitted as if statements
   *  - anything else is a for-loop
   * @param l a [[Loop]] node.
   */
  private def print(l: Loop) {
    val range = l.indexVar.range.asInstanceOf[RangeAdd]

    val init = range.start
    val cond = range.stop
    val update = range.step

    l.iter match {
      case Cst(0) =>

      case Cst(1) =>
        // exactly one iteration
        printBlock {
          println("int " + print(l.indexVar) + " = " + print(init) + ";")
          print(l.body)
        }

      case IntDiv (Cst(1), x) if x.getClass == ?.getClass =>
        // one or less iteration
        printBlock {
          println("int " + print(l.indexVar) + " = " + print(init) + ";")
          print("if (" + print(l.indexVar) + " < (" + print(cond) + ")) ")
          printBlock {
            print(l.body)
          }
        }

      case _ =>
        // as the default print of the default loop
        print ("for (int " + print (l.indexVar) + " = " +
               print (init) + "; " +
          print (l.indexVar) + " < " + print (cond) + "; " +
          print (l.indexVar) + " += " + print (update) + ") ")
        printBlock {
          print(l.body)
        }
    }
  }
}
