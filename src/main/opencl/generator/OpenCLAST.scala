package opencl.generator

import generic.ast.{GenericAST, PrintContext}
import generic.ast.GenericAST._
import lift.arithmetic.{ArithExpr, Predicate, Var}
import lift.arithmetic.NotEvaluableToIntException._
import ir.{ArrayType, TupleType, Type, VectorType}
import opencl.ir.{AddressSpaceCollection, GlobalMemory, IntPtr, LocalMemory, OpenCLAddressSpace, OpenCLMemory, PrivateMemory, UndefAddressSpace}
import utils.Printer

import scala.language.implicitConversions

object OpenCLAST {

  case class RequiredWorkGroupSize(localSize: NDRange) extends AttributeT {
    override def print(pc: PrintContext): Unit = {
      pc += s"__attribute((reqd_work_group_size(${localSize})))\n"
    }
  }

  /*
   OpenCL specific traits
   */
  trait IsKernel {
    val kernel: Boolean
  }

  trait CLAddressSpace {
    val addressSpace: OpenCLAddressSpace
  }

  /** A function declaration
    *
    * @param name   Name of the function.
    * @param ret    Return type.
    * @param params List of parameter declaration.
    * @param body   Body of the function.
    * @param kernel Flag set if the function is a kernel
    */
  case class OclFunction(name: String,
                         ret: Type,
                         params: List[ParamDeclT],
                         body: MutableBlock,
                         attribute: Option[AttributeT] = None, kernel: Boolean =
                         false) extends
    FunctionT with IsKernel {
    override def print(pc: PrintContext): Unit = {
      if (kernel) pc += "kernel "

      if (attribute.isDefined) attribute.get.print(pc)

      if (kernel) {
        pc += "void"
      } else {
        pc += Printer.toString(ret)
      }

      pc += s" ${name}("
      params.zipWithIndex.foreach({
        case (param, ix) ⇒
          if (ix != 0)
            pc += ", "
          param.print(pc)
      })
      pc += ")"

      if (kernel)
        pc += "{ \n" +
          "#ifndef WORKGROUP_GUARD\n" +
          "#define WORKGROUP_GUARD\n" +
          "#endif\n" +
          "WORKGROUP_GUARD\n"

      body.print(pc)

      if (kernel)
        pc += "}"
    }
  }

  //  case class OclVar(v: lift.arithmetic.Var, t: Type,
  //                    addressSpace: OpenCLAddressSpace) extends VarT with CLAddressSpace


  case class OclVarDecl(v: GenericAST.CVar,
                        t: Type,
                        init: AstNode = null,
                        length: Long = 0,
                        addressSpace: OpenCLAddressSpace = UndefAddressSpace)
    extends VarDeclT with CLAddressSpace {
    override def print(pc: PrintContext): Unit = t match {
      case _: ArrayType =>
        addressSpace match {
          case PrivateMemory =>

            if (length > scala.Int.MaxValue) throw NotEvaluableToInt
            for (i <- 0 until length.toInt)
              pc ++= Printer.toString(Type.getValueType(t)) + " " +
                Printer.toString(v.v) + "_" +
                Printer.toString(i) + ";"

          case LocalMemory if length != 0 =>
            val baseType = Type.getBaseType(t)
            val declaration =
              s"${addressSpace} ${Printer.toString(baseType)} " +
                s"${Printer.toString(v.v)}[${length}]"

            // Make sure the memory is correctly aligned when using pointer casts
            // for forcing vector loads on NVIDIA.
            val optionalAttribute =
            if (UseCastsForVectors()) " __attribute__ ((aligned(16)));" else ";"

            val fullDeclaration = declaration + optionalAttribute

            pc += fullDeclaration

          case _ =>
            val baseType = Type.getBaseType(t)
            pc += s"${addressSpace} ${Printer.toString(baseType)} " +
              s"*${Printer.toString(v.v)}"
            if (init != null) {
              pc += s" = "
              init.print(pc)
            }
            pc += "; "
        }

      case _ =>
        // hackily add support for global memory pointers, but _only_ pointers
        t match {
          case IntPtr =>
            if (addressSpace == GlobalMemory)
              pc += addressSpace + " "
          case _      =>
        }
        if (addressSpace == LocalMemory)
          pc += addressSpace + " "
        pc += s"${Printer.toString(t)} ${Printer.toString(v.v)}"
        if (init != null) {
          pc += s" = "
          init.print(pc)
        }
        pc += "; "
    }
  }

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    */
  case class OclParamDecl(name: String, t: Type,
                          const: Boolean = false,
                          addressSpace: OpenCLAddressSpace = UndefAddressSpace)
    extends ParamDeclT with CLAddressSpace {
    override def print(pc: PrintContext): Unit = t match {
      case ArrayType(_) =>
        // Const restricted pointers to read-only global memory. See issue #2.
        val (consts, restrict) = if (const) ("const ", "restrict ") else ("",
          "")
        pc += consts + addressSpace + " " + Printer.toString(Type.devectorize
        (t)) +
          " " + restrict + name

      case _ =>
        pc += Printer.toString(t) + " " + name
    }
  }


  case class OclLoad(v: VarRef,
                     t: Type,
                     offset: ArithExpression,
                     addressSpace: OpenCLAddressSpace) extends LoadT
    with CLAddressSpace {

    override def print(pc: PrintContext): Unit = {
      if (!UseCastsForVectors()) {
        pc += s"vload${Type.getLength(t)}("
        offset.print(pc)
        pc += ","
        v.print(pc)
        pc += ")"
      } else {
        pc += s"*( ((${addressSpace} ${t}*)"
        v.print(pc)
        pc += s") + "
        offset.print(pc)
        pc += ")"
      }
    }
  }

  // TODO: Can we just get the address space from the var ref?
  case class OclStore(v: VarRef,
                      t: Type,
                      value: AstNode,
                      offset: ArithExpression,
                      addressSpace: OpenCLAddressSpace) extends StoreT
    with CLAddressSpace {
    override def print(pc: PrintContext): Unit = {
      if (!UseCastsForVectors()) {
        pc += s"vstore${Type.getLength(t)}("
        value.print(pc)
        pc += ","
        offset.print(pc)
        pc += ","
        v.print(pc)
        pc += ");"
      } else {
        pc += s"*( ((${addressSpace} ${t}*)"
        v.print(pc)
        pc += s") + "
        offset.print(pc)
        pc += ") = "
        value.print(pc)
      }
    }
  }

  case class OclPointerCast(v: VarRef, t: Type,
                           addressSpace: OpenCLAddressSpace) extends CastT with CLAddressSpace {
    override def print(pc: PrintContext): Unit = {
      pc += "("
      pc += s"(${addressSpace} ${t}*)"
      pc += Printer.toString(v.v.v)
      pc += ")"
      if (v.arrayIndex != null) {
        pc += "["
        v.arrayIndex.print(pc)
        pc += "]"
      }
      if (v.suffix != null) {
        pc += v.suffix
      }
    }
  }


  case class VectorLiteral(t: VectorType, vs: VarRef*) extends ExpressionT {
    override def print(pc: PrintContext): Unit = {
      pc += s"(${t})("
      vs.zipWithIndex.foreach({
        case (ref, ix) ⇒
          if (ix != 0)
            pc += ", "
          ref.print(pc)
      })
      pc += ")"
    }
  }


  /** Inline native code block. Used mainly for UserFun, which are currently
    * represented as strings
    *
    * @param code Native code to insert
    */
  case class OclCode(code: String) extends RawCodeT

  case class OclExtension(content: String) extends StatementT with BlockMember {
    override def print(pc: PrintContext): Unit = {
      pc ++= s"#pragma OPENCL EXTENSION ${content} : enable"
    }
  }

  case class OclBarrier(mem: OpenCLMemory) extends StatementT with BlockMember {
    override def print(pc: PrintContext): Unit = pc += (mem.addressSpace match {
      case GlobalMemory => "barrier(CLK_GLOBAL_MEM_FENCE);"
      case LocalMemory  => "barrier(CLK_LOCAL_MEM_FENCE);"

      case collection: AddressSpaceCollection
        if collection.containsAddressSpace(GlobalMemory) &&
          !collection.containsAddressSpace(LocalMemory) =>
        "barrier(CLK_GLOBAL_MEM_FENCE);"

      case collection: AddressSpaceCollection
        if collection.containsAddressSpace(LocalMemory) &&
          !collection.containsAddressSpace(GlobalMemory) =>
        "barrier(CLK_LOCAL_MEM_FENCE);"

      case _ => "barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);"
    })
  }
}
