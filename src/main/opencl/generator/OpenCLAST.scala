package opencl.generator

import core.generator.GenericAST
import core.generator.GenericAST._
import core.generator.PrettyPrinter._
import lift.arithmetic.NotEvaluableToIntException._
import ir.{ArrayType, Type, VectorType}
import opencl.ir.{AddressSpaceCollection, GlobalMemory, IntPtr, LocalMemory, OpenCLAddressSpace, OpenCLMemory, PrivateMemory, UndefAddressSpace}
import utils.Printer

import scala.language.implicitConversions

object OpenCLAST {


  case class RequiredWorkGroupSize(localSize: NDRange) extends AttributeT {
    override def print(): Doc = {
      s"__attribute((reqd_work_group_size($localSize)))" <> line
    }

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
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

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      OclFunction(name, ret, params.map(_.visitAndRebuild(pre, post).asInstanceOf[ParamDeclT]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableBlock],
        attribute match {
          case Some(a) => Some(a.visitAndRebuild(pre, post).asInstanceOf[AttributeT])
          case None => None
        }, kernel)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      params.foreach(_.visitBy(pre,post))
      body.visitBy(pre,post)
      attribute match {
          case Some(a) => a.visitBy(pre, post)
          case None =>
        }
    }

    override def print(): Doc = {
      val kdescrB = if (kernel) {
        text("kernel ")
      } else {
        empty
      }

      val attrB = if (attribute.isDefined) {
        attribute.get.print()
      } else {
        empty
      }

      val typeB = if (kernel) {
        "void"
      } else {
        Printer.toString(ret)
      }

      val defB = s" $name(" <> intersperse(params.map(_.print())) <> ")"

      val innerB = if (kernel) {
        bracket("{",
          "#ifndef WORKGROUP_GUARD" </>
            "#define WORKGROUP_GUARD" </>
            "#endif" </>
            "WORKGROUP_GUARD" </>
            body.print,
          "}")
      } else {
        body.print()
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

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      OclVarDecl(v.visitAndRebuild(pre, post).asInstanceOf[GenericAST.CVar], t,
        init match {
          case Some(i) => Some(i.visitAndRebuild(pre, post))
          case None => None
        }, length, addressSpace)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      init match {
        case Some(i) => i.visitBy(pre, post)
        case None =>
      }
    }

    override def print(): Doc = t match {
      case _: ArrayType =>
        addressSpace match {
          case PrivateMemory =>
            if (length > scala.Int.MaxValue) throw NotEvaluableToInt
            stack(List.tabulate(length.toInt)(i ⇒ {
              Printer.toString(Type.getValueType(t)) <+> Printer.toString(v
                .v) <> "_" <> Printer.toString(i) <> ";"
            }))  /*** unroll private memory ***/

          case LocalMemory if length != 0 =>
            val baseType = Type.getBaseType(t)
            val declaration =
              s"$addressSpace ${Printer.toString(baseType)} " +
                s"${Printer.toString(v.v)}[$length]"

            // Make sure the memory is correctly aligned when using pointer casts
            // for forcing vector loads on NVIDIA.
            val optionalAttribute =
            if (UseCastsForVectors()) " __attribute__ ((aligned(16)));" else ";"

            val fullDeclaration = declaration + optionalAttribute

            fullDeclaration

          case _ =>
            val baseType = Type.getBaseType(t)
            (s"$addressSpace ${Printer.toString(baseType)} " +
              s"*${Printer.toString(v.v)}") <>
              (init match {
                case Some(i) ⇒ " = " <> i.print()
                case None    ⇒ empty
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
              empty
            }
          case _      => empty
        }) <>
          (if (addressSpace == LocalMemory) {
            addressSpace.toString <> " "
          } else {
            empty
          }) <> s"${Printer.toString(t)} ${Printer.toString(v.v)}" <>
          (init match {
            case Some(i) ⇒ " = " <> i.print()
            case None    ⇒ empty
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

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}

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
                     shift: ArithExpression,
                     addressSpace: OpenCLAddressSpace) extends LoadT
    with CLAddressSpace {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      OclLoad(v.visitAndRebuild(pre, post).asInstanceOf[VarRef], t,
        offset.visitAndRebuild(pre, post).asInstanceOf[ArithExpression],
        shift.visitAndRebuild(pre, post).asInstanceOf[ArithExpression],
        addressSpace)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      offset.visitBy(pre, post)
      shift.visitBy(pre, post)
    }

    override def print(): Doc = {
      if (!UseCastsForVectors()) {
        s"vload${Type.getLength(t)}(" <>
          offset.print <>
          "," <>
          v.print <>
          " + " <>
          shift.print <>
          ")"
      } else {
        s"*( (($addressSpace $t*)" <>
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

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      OclStore(v.visitAndRebuild(pre, post).asInstanceOf[VarRef], t,
        value.visitAndRebuild(pre, post),
        offset.visitAndRebuild(pre, post).asInstanceOf[ArithExpression],
        addressSpace)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      value.visitBy(pre, post)
      offset.visitBy(pre, post)
    }

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
        s"*( (($addressSpace $t*)" <>
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

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      OclPointerCast(v.visitAndRebuild(pre, post).asInstanceOf[VarRef], t, addressSpace)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
    }

    override def print(): Doc = {
      "(" <> s"($addressSpace $t*)" <> Printer.toString(v.v.v) <> ")" <>
        (v.arrayIndex match {
          case None ⇒ empty
          case Some(ix)    ⇒ "[" <> ix.print <> "]"
        }) <>
        (v.suffix match {
          case None ⇒ empty
          case Some(sf)    ⇒ text(sf)
        })
    }
  }


  case class VectorLiteral(t: VectorType, vs: VarRef*) extends ExpressionT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      VectorLiteral(t, vs.map(_.visitAndRebuild(pre, post).asInstanceOf[VarRef]) : _*)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      vs.map(_.visitBy(pre, post))
    }

    override def print(): Doc = {
      s"($t)(" <>
        intersperse(vs.map(_.print()).toList) <>
        ")"
    }
  }


  /** Inline native code block. Used mainly for UserFun, which are currently
    * represented as strings
    *
    * @param code Native code to insert
    */
  case class OclCode(code: String, pre1: String = "", pre2: String ="", post1: String ="", post2: String = "") extends RawCodeT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}

  }

  case class OclExtension(content: String) extends StatementT with BlockMember {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}

    override def print(): Doc = {
      "#pragma OPENCL EXTENSION " <> content <> " : enable"
    }
  }

  case class OclBarrier(mem: OpenCLMemory) extends StatementT with BlockMember {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}

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
