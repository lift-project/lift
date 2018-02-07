//package opencl.generator
//
//import ir._
//import lift.arithmetic.NotEvaluableToIntException._
//import lift.arithmetic._
//import opencl.generator.OpenCLAST._
//import opencl.ir._
//import generic.ast.GenericAST._
//import utils.Printer
//
//object OpenCLPrinter {
//  def apply() = new OpenCLPrinter
//}
//
///** The printer walks the AST emitted by the [[OpenCLGenerator]] and generates
// * standalone OpenCL-C code.
// * standalone OpenCL-C code.
// */
//class OpenCLPrinter extends Printer {
//  /**
//   * Entry point for printing an AST.
//   *
//   * @param node The root of the AST (the global scope block).
//   * @return A string representation of the AST as OpenCL-C code.
//   */
//  def apply(node: AstNode): String = {
//    indent = 0
//    print(node)
//
//    sb.toString()
//  }
//
//  /**
//   * Main print method. Print the current node and recurse.
//   *
//   * @param node The current node to emit code for.
//   */
//  private def print(node: AstNode): Unit = node match {
//    case b: Block =>
//      if (b.global) {
//        b.content.foreach(
//          n => {
//            print(n)
//            println("")
//          }
//        )
//
//      }
//      else printBlock {
//        b.content.foreach(n => {
//          print(n)
//          println("")
//        })
//      }
//
//    case f: OclFunction              => print(f)
//    case a: RequiredWorkGroupSize => print(a)
//    case i: OclCode            => sb ++= i.code
//    case e: OpenCLExpression      => sb ++= e.code
//    case c: Comment               => print(s"/* ${c.content} */")
//    case v: VarDecl               => print(v)
//    case v: VarRef                => print(v)
//    case p: ParamDecl             => print(p)
//    case b: Barrier               => print(b)
//    case l: ForLoop               => print(l)
//    case w: WhileLoop             => print(w)
//    case es: ExpressionStatement  => print(es)
//    case ae: ArithExpression      => print(Printer.toString(ae.content))
//    case c: BinaryExpression      => print(c)
//    case t: TernaryExpression     => print(t)
//    case a: AssignmentExpression  => print(a)
//    case f: FunctionCall          => print(f)
//    case l: Load                  => print(l)
//    case s: Store                 => print(s)
//    case t: TypeDef               => print(t)
//    case a: TupleAlias            => print(a)
//    case c: Cast                  => print(c)
//    case c: PointerCast           => print(c)
//    case l: VectorLiteral         => print(l)
//    case e: OclExtension       => print(e)
//    case i: OpenCLAST.IfThenElse  => print(i)
//    case l: Label                 => print(l)
//    case g: GOTO                  => print(g)
//    case b: Break                 => print(b)
//    case s: StructConstructor     => print(s)
//
//    case x => print(s"/* UNKNOWN: ${x.getClass.getSimpleName} */")
//  }
//
//  private def print(c: BinaryExpression): Unit = {
//    print("(")
//    print(c.lhs)
//    print(s" ${c.op.toString} ")
//    print(c.rhs)
//    print(")")
//  }
//
//  private def print(t: TernaryExpression): Unit = {
//    print("(")
//    print(t.cond)
//    print(" ? ")
//    print(t.trueExpr)
//    print(" : ")
//    print(t.falseExpr)
//    print(")")
//  }
//
//  private def print(c: Cast): Unit = {
//    print(s"(${c.t})")
//    print(c.v)
//  }
//
//  private def print(c: PointerCast): Unit = {
//    print("(")
//    print(s"(${c.addressSpace} ${c.t}*)")
//    print(Printer.toString(c.v.v))
//    print(")")
//    if (c.v.arrayIndex != null) {
//      print("[")
//      print(c.v.arrayIndex)
//      print("]")
//    }
//    if (c.v.suffix != null) {
//      print(c.v.suffix)
//    }
//  }
//
//  private def print(l: VectorLiteral): Unit = {
//    print(s"(${l.t})(")
//    printList(l.vs, ", ")
//    print(")")
//  }
//
//  private def print(t: TypeDef): Unit = t.t match {
//    case tt: TupleType =>
//      tt.elemsT.foreach(t => print(TypeDef(t)))
//      val name = Type.name(tt)
//      val fields = tt.elemsT.zipWithIndex.map({ case (ty, i) => Type.name(ty) + " _" + i })
//      print(
//        s"""#ifndef ${name}_DEFINED
//           |#define ${name}_DEFINED
//           |typedef struct __attribute__((aligned(${tt.alignment._1}))) {
//           |  ${fields.reduce(_ + ";\n  " + _)};
//           |} $name;
//           |#endif
//           |""".stripMargin)
//    case _             =>
//  }
//
//  private def print(e: OclExtension): Unit = {
//    println(s"#pragma OPENCL EXTENSION ${e.content} : enable")
//  }
//
//  private def print(alias: TupleAlias): Unit = alias.t match {
//    case tt: TupleType =>
//      println(s"typedef ${Type.name(tt)} ${alias.name};")
//    case _             =>
//  }
//
//  private def print(l: Load): Unit = {
//    if (!UseCastsForVectors()) {
//      print(s"vload${l.t.len}(")
//      print(l.offset)
//      print(",")
//      print(l.v)
//      print(")")
//    } else {
//      print(s"*( ((${l.openCLAddressSpace} ${l.t}*)")
//      print(l.v)
//      print(s") + ")
//      print(l.offset)
//      print(")")
//    }
//  }
//
//  private def print(s: Store): Unit = {
//    if (!UseCastsForVectors()) {
//      print(s"vstore${s.t.len}(")
//      print(s.value)
//      print(",")
//      print(s.offset)
//      print(",")
//      print(s.v)
//      print(");")
//    } else {
//      print(s"*( ((${s.openCLAddressSpace} ${s.t}*)")
//      print(s.v)
//      print(s") + ")
//      print(s.offset)
//      print(") = ")
//      print(s.value)
//    }
//  }
//
//  private def print(f: FunctionCall): Unit = {
//    print(f.name + "(")
//    printList(f.args, ", ")
//    print(")")
//  }
//
//  private def print(v: VarRef): Unit = {
//    print(Printer.toString(v.v))
//    if (v.arrayIndex != null) {
//      print("[")
//      print(v.arrayIndex)
//      print("]")
//    }
//    if (v.suffix != null) {
//      print(v.suffix)
//    }
//  }
//
//  private def print(a: RequiredWorkGroupSize): Unit = {
//    val localSize = a.localSize
//    sb ++=
//      s"__attribute((reqd_work_group_size($localSize)))\n"
//  }
//
//  private def print(f: OclFunction): Unit = {
//    if (f.kernel) sb ++= "kernel "
//
//    if (f.attribute.isDefined) print(f.attribute.get)
//
//    if (f.kernel) sb ++= "void"
//    else sb ++= Printer.toString(f.ret)
//    print(s" ${f.name}(")
//    printList(f.params, ", ")
//    print(")")
//
//    if (f.kernel)
//      sb ++= "{ \n" +
//        "#ifndef WORKGROUP_GUARD\n" +
//        "#define WORKGROUP_GUARD\n" +
//        "#endif\n" +
//        "WORKGROUP_GUARD\n"
//
//    print(f.body)
//
//    if (f.kernel)
//      println("}")
//  }
//
//  private def print(es: ExpressionStatement): Unit = {
//    print(es.e)
//    print("; ")
//  }
//
//
//  private def print(a: AssignmentExpression): Unit = {
//    print(a.to)
//    print(" = ")
//    print(a.value)
//  }
//
//  private def print(p: ParamDecl): Unit = p.t match {
//    case ArrayType(_) =>
//      // Const restricted pointers to read-only global memory. See issue #2.
//      val (const, restrict) = if (p.const) ("const ", "restrict ") else ("", "")
//      print(const + p.addressSpace + " " + Printer.toString(Type.devectorize(p.t)) +
//        " " + restrict + p.name)
//
//    case _ =>
//      print(Printer.toString(p.t) + " " + p.name)
//  }
//
//
//  private def print(vd: VarDecl): Unit = vd.t match {
//    case _: ArrayType =>
//      vd.addressSpace match {
//        case PrivateMemory =>
//          if (vd.length > scala.Int.MaxValue) throw NotEvaluableToInt
//          for (i <- 0 until vd.length.toInt)
//            println(Printer.toString(Type.getValueType(vd.t)) + " " +
//              Printer.toString(vd.v) + "_" +
//              Printer.toString(i) + ";")
//
//        case LocalMemory if vd.length != 0 =>
//          val baseType = Type.getBaseType(vd.t)
//          val declaration =
//            s"${vd.addressSpace} ${Printer.toString(baseType)} " +
//              s"${Printer.toString(vd.v)}[${vd.length}]"
//
//          // Make sure the memory is correctly aligned when using pointer casts
//          // for forcing vector loads on NVIDIA.
//          val optionalAttribute =
//          if (UseCastsForVectors()) " __attribute__ ((aligned(16)));" else ";"
//
//          val fullDeclaration = declaration + optionalAttribute
//
//          print(fullDeclaration)
//
//        case _ =>
//          val baseType = Type.getBaseType(vd.t)
//          print(s"${vd.addressSpace} ${Printer.toString(baseType)} " +
//            s"*${Printer.toString(vd.v)}")
//          if (vd.init != null) {
//            print(s" = ")
//            print(vd.init)
//          }
//          print("; ")
//      }
//
//    case _ =>
//      // hackily add support for global memory pointers, but _only_ pointers
//      vd.t match {
//        case IntPtr =>
//          if (vd.addressSpace == GlobalMemory)
//            print(vd.addressSpace + " ")
//        case _      =>
//      }
//      if (vd.addressSpace == LocalMemory)
//        print(vd.addressSpace + " ")
//      print(s"${Printer.toString(vd.t)} ${Printer.toString(vd.v)}")
//      if (vd.init != null) {
//        print(s" = ")
//        print(vd.init)
//      }
//      print("; ")
//  }
//
//  /**
//   * Generate a barrier for the given address space scope.
//   * If the scope is not defined as global or local, the barrier assumes both.
//   *
//   * @param b A [[Barrier]] node.
//   */
//  private def print(b: Barrier): Unit = println(b.mem.addressSpace match {
//    case GlobalMemory => "barrier(CLK_GLOBAL_MEM_FENCE);"
//    case LocalMemory  => "barrier(CLK_LOCAL_MEM_FENCE);"
//
//    case collection: AddressSpaceCollection
//      if collection.containsAddressSpace(GlobalMemory) &&
//        !collection.containsAddressSpace(LocalMemory) =>
//      "barrier(CLK_GLOBAL_MEM_FENCE);"
//
//    case collection: AddressSpaceCollection
//      if collection.containsAddressSpace(LocalMemory) &&
//        !collection.containsAddressSpace(GlobalMemory) =>
//      "barrier(CLK_LOCAL_MEM_FENCE);"
//
//    case _ => "barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);"
//  })
//
//  /**
//   * Generate a for loop.
//   *
//   * @param fl a [[ForLoop]] node.
//   */
//  private def print(fl: ForLoop): Unit = {
//    print("for (")
//    print(fl.init)
//    print(fl.cond)
//    print(fl.increment)
//    print(") ")
//    print(fl.body)
//  }
//
//
//  /**
//   * Generate a while loop. This is fairly simple so no
//   * optimisations can be realistically applied.
//   *
//   * @param wl a [[WhileLoop]] node.
//   */
//  private def print(wl: WhileLoop): Unit = {
//    print("while(" + Printer.toString(wl.loopPredicate) + ")")
//    print(wl.body)
//  }
//
//
//  /** Generate an if-then-else conditional set of statements
//   *
//   * @param s a [[IfThenElse]] node
//   */
//  private def print(s: OpenCLAST.IfThenElse): Unit = {
//    print("if (")
//    print(s.cond)
//    print(") ")
//
//    print(s.trueBody)
//
//    if (s.falseBody != Block()) {
//      print(" else ")
//      print(s.falseBody)
//    }
//  }
//
//  /** Generate a label for a goto
//   *
//   * @param l a [[Label]] node
//   */
//  private def print(l: Label): Unit = {
//    println(l.nameVar.toString + ": ;")
//  }
//
//  /** Generate a goto statement for a corresponding label
//   *
//   * @param g a [[GOTO]] node
//   */
//  private def print(g: GOTO): Unit = {
//    println("goto " + g.nameVar.toString + ";")
//  }
//
//  private def print(b: Break): Unit = {
//    print("break;")1
//  }
//
//  private def print(s: StructConstructor): Unit = {
//    print(s"(${Printer.toString(s.t)}){")
//    printList(s.args, ", ")
//    print("}")
//  }
//
//  /**
//   * Helper function for printing separated lists
//   * `printList([a, b, c], ",")  ==  "a,b,c"`
//   */
//  private def printList(args: Seq[OclAstNode], sep: String): Unit = {
//    args.init.foreach(a => {
//      print(a)
//      print(sep)
//    })
//    print(args.last)
//  }
//
//}
