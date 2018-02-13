package opencl.generator

import generic.ast.{GenericAST, PrintContext}
import generic.ast.GenericAST._
import generic.ast.PrettyPrinter._
import lift.arithmetic.{ArithExpr, Predicate, Var}
import lift.arithmetic.NotEvaluableToIntException._
import ir.{ArrayType, TupleType, Type, VectorType}
import opencl.ir.{AddressSpaceCollection, GlobalMemory, IntPtr, LocalMemory, OpenCLAddressSpace, OpenCLMemory, PrivateMemory, UndefAddressSpace}
import utils.Printer

import scala.language.implicitConversions

object OpenCLAST {

  case class RequiredWorkGroupSize(localSize: NDRange) extends AttributeT {
    override def print(): Doc = {
      s"__attribute((reqd_work_group_size(${localSize})))" <> line
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
    override def print(): Doc = {
      val kdescrB = if (kernel) {
        text("kernel ")
      } else {
        nil
      }

      val attrB = if (attribute.isDefined) {
        attribute.get.print
      } else {
        nil
      }

      val typeB = if (kernel) {
        "void"
      } else {
        Printer.toString(ret)
      }

      val defB = s" ${name}(" <> intersperse(params.map(_.print)) <> ")"

      val innerB = if (kernel) {
        bracket("{",
          "#ifndef WORKGROUP_GUARD" </>
            "#define WORKGROUP_GUARD" </>
            "#endif" </>
            "WORKGROUP_GUARD" </>
            body.print,
          "}")
      } else {
        body.print
      }

      kdescrB <> attrB <> typeB <> defB <> innerB

    }
  }

  //  case class OclVar(v: lift.arithmetic.Var, t: Type,
  //                    addressSpace: OpenCLAddressSpace) extends VarT with CLAddressSpace


  case class OclVarDecl(v: GenericAST.CVar,
                        t: Type,
                        init: Option[AstNode] = None,
                        length: Long = 0,
                        addressSpace: OpenCLAddressSpace = UndefAddressSpace)
    extends VarDeclT with CLAddressSpace {

    override def print(): Doc = t match {
      case _: ArrayType =>
        addressSpace match {
          case PrivateMemory =>
            if (length > scala.Int.MaxValue) throw NotEvaluableToInt
            stack(List.tabulate(length.toInt)(i ⇒ {
              Printer.toString(Type.getValueType(t)) <+> Printer.toString(v
                .v) <> "_" <> Printer.toString(i) <> ";"
            }))

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

            fullDeclaration

          case _ =>
            val baseType = Type.getBaseType(t)
            (s"${addressSpace} ${Printer.toString(baseType)} " +
              s"*${Printer.toString(v.v)}") <>
              (init match {
                case Some(i) ⇒ " = " <> i.print()
                case None    ⇒ nil
              }) <>
              "; "
        }

      case _ =>
        // hackily add support for global memory pointers, but _only_ pointers
        (t match {
          case IntPtr =>
            if (addressSpace == GlobalMemory) {
              addressSpace.toString <> " "
            } else {
              nil
            }
          case _      => nil
        }) <>
          (if (addressSpace == LocalMemory) {
            addressSpace.toString <> " "
          } else {
            nil
          }) <> s"${Printer.toString(t)} ${Printer.toString(v.v)}" <>
          (init match {
            case Some(i) ⇒ " = " <> i.print()
            case None    ⇒ nil
          }) <>
          "; "
    }
  }

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    */
  case class OclParamDecl(name: String, t: Type,
                          const: Boolean = false,
                          addressSpace: OpenCLAddressSpace = UndefAddressSpace)
    extends ParamDeclT with CLAddressSpace {
    override def print(): Doc = t match {
      case ArrayType(_) ⇒
        // Const restricted pointers to read-only global memory. See issue #2.
        val (consts, restrict) = if (const) ("const ", "restrict ") else ("",
          "")

        consts <> addressSpace.toString <+> Printer.toString(Type.devectorize
        (t)) <+> restrict <> name

      case _ ⇒
        Printer.toString(t) <+> name
    }
  }


  case class OclLoad(v: VarRef,
                     t: Type,
                     offset: ArithExpression,
                     addressSpace: OpenCLAddressSpace) extends LoadT
    with CLAddressSpace {
    override def print(): Doc = {
      if (!UseCastsForVectors()) {
        s"vload${Type.getLength(t)}(" <>
          offset.print <>
          "," <>
          v.print <>
          ")"
      } else {
        s"*( ((${addressSpace} ${t}*)" <>
          v.print <>
          s") + " <>
          offset.print <>
          ")"
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
    override def print(): Doc = {
      if (!UseCastsForVectors()) {
        s"vstore${Type.getLength(t)}(" <>
          value.print() <>
          "," <>
          offset.print() <>
          "," <>
          v.print <>
          ")"
      } else {
        s"*( ((${addressSpace} ${t}*)" <>
          v.print <>
          s") + " <>
          offset.print <>
          ") = " <>
          value.print
      }
    }
  }

  case class OclPointerCast(v: VarRef, t: Type,
                            addressSpace: OpenCLAddressSpace) extends CastT with CLAddressSpace {

    override def print(): Doc = {
      "(" <> s"(${addressSpace} ${t}*)" <> Printer.toString(v.v.v) <> ")" <>
        (v.arrayIndex match {
          case None ⇒ nil
          case Some(ix)    ⇒ "[" <> ix.print <> "]"
        }) <>
        (v.suffix match {
          case None ⇒ nil
          case Some(sf)    ⇒ text(sf)
        })
    }
  }


  case class VectorLiteral(t: VectorType, vs: VarRef*) extends ExpressionT {
    override def print(): Doc = {
      s"(${t})(" <>
        intersperse(vs.map(_.print).toList) <>
        ")"
    }
  }


  /** Inline native code block. Used mainly for UserFun, which are currently
    * represented as strings
    *
    * @param code Native code to insert
    */
  case class OclCode(code: String) extends RawCodeT

  case class OclExtension(content: String) extends StatementT with BlockMember {
    override def print(): Doc = {
      "#pragma OPENCL EXTENSION " <> content <> " : enable"
    }
  }

  case class OclBarrier(mem: OpenCLMemory) extends StatementT with BlockMember {
    override def print(): Doc = mem.addressSpace match {
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
    }
  }

}
