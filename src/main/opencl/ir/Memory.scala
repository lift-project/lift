package opencl.ir

import java.io.{File, PrintWriter}

import apart.arithmetic._
import arithmetic.TypeVar
import ir._
import ir.ast._
import ir.printer.DotPrinter
import opencl.ir.pattern._

/** Represents OpenCL address spaces either: local or global;
  * UndefAddressSpace should be used in case of errors */
abstract class OpenCLAddressSpace

object LocalMemory extends OpenCLAddressSpace {
  override def toString = "local"
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString = "global"
}

object PrivateMemory extends OpenCLAddressSpace {
  override def toString = "private"
}

object MixedMemory extends  OpenCLAddressSpace {
  override def toString = "mixed"
}

/**
  * Used for function which do not need any memory
  */
object NoMemory extends OpenCLAddressSpace {
  override def toString = "no memory"
}

object UndefAddressSpace extends OpenCLAddressSpace

class UnexpectedAddressSpaceException(val found: String, val expected: String) extends Exception("Found "+found+" expected "+expected)

object OpenCLAddressSpace {

  private def addAddressSpace(e: Expr, as : OpenCLAddressSpace) : scala.collection.Set[OpenCLAddressSpace] = {
    //Type.visit(t, {t => t.addressSpace = as})
    e.addressSpaces += as
    e.addressSpaces
  }

  private def addAddressSpace(e: Expr, as : scala.collection.Set[OpenCLAddressSpace]) : scala.collection.Set[OpenCLAddressSpace] = {
    //Type.visit(t, {t => t.addressSpace = as})
    e.addressSpaces ++= as
    e.addressSpaces
  }

  // by default the parameters of the lambda will be set to global space and the return address space should be in global memory
  def setAddressSpace(l: Lambda) : Unit = {

    //new DotPrinter(new PrintWriter(new File("/home/cdubach/graph1.dot")), false, false,true).print(l)

    // clear up all the address spaces first
    IRNode.visit(l, _ match {
        case e: Expr => e.addressSpaces.clear
        case _ =>
      }
    )

    // set the param address space to global memory
    l.params.foreach(p => addAddressSpace(p, GlobalMemory))
    setAddressSpace(l.body)

    //new DotPrinter(new PrintWriter(new File("/home/cdubach/graph.dot")), false, true).print(l)


    if (l.body.addressSpaces.size != 1 | ! l.body.addressSpaces.contains(GlobalMemory) )
      throw new UnexpectedAddressSpaceException(l.body.addressSpaces.toString, GlobalMemory.toString)
  }

  private def setAddressSpaceLambda(l: Lambda, writeTo : OpenCLAddressSpace, argsAddrSpace : Seq[Set[OpenCLAddressSpace]]) = {
    l.params.zip(argsAddrSpace).foreach({case (p,a) =>
      assert (p.addressSpaces.isEmpty)
      p.addressSpaces ++= a
    })
    setAddressSpace(l.body, writeTo)
  }

  private def inferFunCallWriteTo(writeTo : OpenCLAddressSpace, args: Set[Expr]) = {
    if (writeTo == UndefAddressSpace) {
      val addSpaces = args.map(_.addressSpaces).reduce(_ ++ _)
      if (addSpaces.contains(GlobalMemory))
        GlobalMemory
      else if (addSpaces.contains(LocalMemory))
        LocalMemory
      else if (addSpaces.contains(PrivateMemory))
        PrivateMemory
      else
        UndefAddressSpace
    } else
      writeTo
  }

  private def setAddressSpace(e: Expr, writeTo : OpenCLAddressSpace = UndefAddressSpace) : scala.collection.Set[OpenCLAddressSpace] = {
     val retAS = e match {
      case Value(_) =>
        assert (writeTo == PrivateMemory | writeTo == UndefAddressSpace | writeTo == NoMemory)
        addAddressSpace(e, PrivateMemory) // note that the address space of a Value may be already set to Private (the same Value can appear multiple times in the IR graph)

      case p: Param =>
        assert (p.addressSpaces.nonEmpty)
        p.addressSpaces.toSet

      case f: FunCall =>
        f.args.foreach(setAddressSpace(_))
        f.f match {
          case l: Lambda =>
            addAddressSpace(f, writeTo)
            setAddressSpaceLambda(l, writeTo, f.args.map(_.addressSpaces.toSet))
          case t: toLocal =>
            addAddressSpace(f, LocalMemory)
            setAddressSpaceLambda(t.f, LocalMemory, f.args.map(_.addressSpaces.toSet))
          case t: toPrivate =>
            addAddressSpace(f, PrivateMemory)
            setAddressSpaceLambda(t.f, PrivateMemory, f.args.map(_.addressSpaces.toSet))
          case t: toGlobal =>
            addAddressSpace(f, GlobalMemory)
            setAddressSpaceLambda(t.f, GlobalMemory, f.args.map(_.addressSpaces.toSet))
          case r: AbstractPartRed =>
            assert (f.args(0).addressSpaces.size == 1) // first argument is initial value
            val writeTo = f.args(0).addressSpaces.toList(0) // the address space of the result of a reduction is always the same as the initial element
            addAddressSpace(f, writeTo)
            setAddressSpaceLambda(r.f, writeTo, f.args.map(_.addressSpaces.toSet))
          case fp : FPattern =>
            val inferredWriteTo : OpenCLAddressSpace = inferFunCallWriteTo(writeTo, f.args.toSet)
            addAddressSpace(f, inferredWriteTo)
            setAddressSpaceLambda(fp.f, inferredWriteTo, f.args.map(_.addressSpaces.toSet))
          case VectorizeUserFun(_,_) | UserFun(_, _, _, _, _) | Unzip() | Zip(_) | Transpose() | TransposeW() | asVector(_) | asScalar() | Split(_) | Join() | Scatter(_) | Gather(_) | Pad(_, _) | Tuple(_) | Group(_) | Filter() | Head() | Tail() | Get(_) =>
            val inferredWriteTo : OpenCLAddressSpace = inferFunCallWriteTo(writeTo, f.args.toSet)
            assert (e.addressSpaces.isEmpty)
            addAddressSpace(f, inferredWriteTo)
        }

    }
    retAS
  }


}

case class AddressSpaceCollection(spaces: Seq[OpenCLAddressSpace])
  extends OpenCLAddressSpace {

  def findCommonAddressSpace(): OpenCLAddressSpace = {
    // try to find common address space which is not the private memory ...
    val noPrivateMem = spaces.filterNot(_== PrivateMemory)
    if (noPrivateMem.isEmpty) { // everything is in private memory
      return PrivateMemory
    }

    val addessSpaces = noPrivateMem.map({
      case coll: AddressSpaceCollection => coll.findCommonAddressSpace()
      case space => space
    })

    if (addessSpaces.forall(_ == addessSpaces.head)) {
      addessSpaces.head
    } else {
      // FIXME(tlutz): Document that the default address space is global when the tuple has mixed addess spaces.
      GlobalMemory
      //throw new IllegalArgumentException(s"Could not determine common addressSpace: $addessSpaces")
    }
  }
}


/** Represents memory in OpenCL as a raw collection of bytes allocated in an
  * OpenCL address space.
  *
  * @constructor Create a new OpenCLMemory object
  * @param variable The variable associated with the memory
  * @param size The size of the memory as numbers bytes
  * @param addressSpace The address space where the memory has been allocated
  */
class OpenCLMemory(var variable: Var,
                   val size: ArithExpr,
                   val addressSpace: OpenCLAddressSpace) extends Memory {

  // size cannot be 0 unless it is the null memory
  try {
    if (size.eval == 0)
      throw new IllegalArgumentException("Cannot have a memory of 0 bytes!")
  } catch {
    case _: NotEvaluableException => // nothing to do
    case e: Exception => throw e
  }

  // no type variable allowed in the size
  if (TypeVar.getTypeVars(size).nonEmpty)
    throw new IllegalArgumentException

  // no unknown allowed in the size
  val hasUnknown = ArithExpr.visitUntil(size, _ == ?)
  if (hasUnknown)
    throw new IllegalArgumentException



  def copy(): OpenCLMemory = {
    addressSpace match {
      case GlobalMemory => OpenCLMemory.allocGlobalMemory(size)
      case LocalMemory => OpenCLMemory.allocLocalMemory(size)
      case PrivateMemory => OpenCLMemory.allocPrivateMemory(size)
      case _ => this
    }
  }

  /** Debug output */
  override def toString: String = {
    this match {
      case coll: OpenCLMemoryCollection =>
        "[" + coll.subMemories.map(_.toString).reduce(_ + ", " + _) + "]"
      case _ =>
        "{" + variable + "; " + addressSpace + "; " + size + "}"
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[OpenCLMemory]

  override def equals(other: Any): Boolean = other match {
    case that: OpenCLMemory =>
      (that canEqual this) &&
        variable == that.variable &&
        size == that.size &&
        addressSpace == that.addressSpace
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(variable, size, addressSpace)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class OpenCLMemoryCollection(val subMemories: Array[OpenCLMemory],
                             override val addressSpace: AddressSpaceCollection)
  extends OpenCLMemory(Var("Tuple"), subMemories.map(_.size).reduce(_+_),
                       addressSpace)

object OpenCLMemoryCollection {
  def apply(mems: Seq[OpenCLMemory]) = {
    val addressSpace = new AddressSpaceCollection(mems.map(_.addressSpace))
    new OpenCLMemoryCollection(mems.toArray, addressSpace)
  }
}

/** Represents the NULL OpenCL memory object */
object OpenCLNullMemory
  extends OpenCLMemory(Var("NULL"), Cst(-1), UndefAddressSpace)


object OpenCLMemory {

  def apply(variable: Var, size: ArithExpr,
            addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    new OpenCLMemory(variable, size, addressSpace)
  }

  def asOpenCLMemory(m: Memory): OpenCLMemory = {
    m match {
      case oclm: OpenCLMemory => oclm
      case UnallocatedMemory => OpenCLNullMemory
      case _ => throw new IllegalArgumentException
    }
  }

  // checking for address spaces
  def containsAddressSpace(mem: Memory,
                           memType: OpenCLAddressSpace): Boolean = {
    mem match {
      case coll: OpenCLMemoryCollection =>
        coll.subMemories.exists(x => x.addressSpace == memType)
      case m: OpenCLMemory => m.addressSpace == memType
      case _ => false
    }
  }

  def containsGlobalMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, GlobalMemory)

  def containsLocalMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, LocalMemory)

  def containsPrivateMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, PrivateMemory)

  /** Return newly allocated memory based on the given sizes and the address
    * space of the input memory
    *
    * @param glbOutSize Size in bytes to allocate in global memory
    * @param lclOutSize Size in bytes to allocate in local memory
    * @param addressSpace Address space for allocation
    * @return The newly allocated memory object
    */
  def allocMemory(glbOutSize: ArithExpr,
                  lclOutSize: ArithExpr,
                  pvtOutSize: ArithExpr,
                  addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    assert(addressSpace != UndefAddressSpace)

    addressSpace match {
      case GlobalMemory => allocGlobalMemory(glbOutSize)
      case LocalMemory => allocLocalMemory(lclOutSize)
      case PrivateMemory => allocPrivateMemory(pvtOutSize)
      case co: AddressSpaceCollection =>
        allocMemory(glbOutSize, lclOutSize, pvtOutSize, co.findCommonAddressSpace())
    }
  }

  /** Return newly allocated global memory */
  def allocGlobalMemory(glbOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(new Var("",ContinuousRange(Cst(0), glbOutSize)),
                 glbOutSize, GlobalMemory)
  }

  /** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(new Var("",ContinuousRange(Cst(0), lclOutSize)),
                 lclOutSize, LocalMemory)
  }

  def allocPrivateMemory(size: ArithExpr): OpenCLMemory = {
    OpenCLMemory(new Var("",ContinuousRange(Cst(0), size)), size, PrivateMemory)
  }

  /*ef getSizeInBytes(t: Type): ArithExpr = {
    //ArithExpr.max(getSizeInBytes(t))
    getSizeInBytes(t).max
  }*/

  def getSizeInBytes(t: Type): ArithExpr = t match {
    case st: ScalarType => st.size
    case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
    case at: ArrayType => at.len * getSizeInBytes(at.elemT)
    case tt: TupleType => tt.elemsT.map(getSizeInBytes).reduce(_ + _)
    case _ => throw new TypeException(t, "??")
  }
}

/** Represents an OpenCLMemory object combined with a type.
  *
  * @constructor Create a new TypedOpenCLMemory object
  * @param mem The underlying memory object
  * @param t The type associated with the memory object
  */
case class TypedOpenCLMemory(mem: OpenCLMemory, t: Type) {
  override def toString = "(" + mem.toString +": " + t.toString + ")"
}

object TypedOpenCLMemory {
  def apply(expr: Expr): TypedOpenCLMemory = {
    new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(expr.mem), expr.t)
  }

  def apply(mem: Memory, t: Type): TypedOpenCLMemory = {
    new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(mem), t)
  }

  def get(expr: Expr,
          params: Seq[Param],
          includePrivate: Boolean = false): Seq[TypedOpenCLMemory] = {

    // nested functions so that `params` and `includePrivate` are in scope

    def collect(expr: Expr): Seq[TypedOpenCLMemory] = {
      expr match {
        case v: Value => collectValue(v)
        case p: Param => Seq()
        case call: FunCall => collectFunCall(call)
      }
    }

    def collectValue(v: Value): Seq[TypedOpenCLMemory] = {
      if (includePrivate) {
        Seq(TypedOpenCLMemory(v))
      } else {
        Seq()
      }
    }

    def collectFunCall(call: FunCall): Seq[TypedOpenCLMemory] = {
      val argMems: Seq[TypedOpenCLMemory] = call.args.length match {
        case 0 => Seq()
        case 1 => collect(call.args.head)
        case _ => call.args.map(collect).reduce(_ ++ _)
      }

      val bodyMems = call.f match {
        case uf: UserFun    => collectUserFun(call)
        case vf: VectorizeUserFun
                            => collectUserFun(call)
        case l: Lambda      => collect(l.body)
        case m: AbstractMap => collectMap(call.t, m)
        case r: AbstractPartRed => collectReduce(r, argMems)
        case s: AbstractSearch => collectSearch(s, call, argMems)
        case i: Iterate     => collectIterate(call, i)
        case fp: FPattern   => collect(fp.f.body)
        case _              => Seq()
      }

      argMems ++ bodyMems
    }

    def collectUserFun(call: FunCall): Seq[TypedOpenCLMemory] = {
      call.mem match {
        case m: OpenCLMemory =>
          if (!includePrivate && m.addressSpace == PrivateMemory) {
            Seq()
          } else {
            Seq(TypedOpenCLMemory(call))
          }
      /*  case UnallocatedMemory =>
          println("error")
          Seq()*/
      }
    }

    def collectMap(t: Type,
                   m: AbstractMap): Seq[TypedOpenCLMemory] = {
      val mems = collect(m.f.body)  

      def changeType(addressSpace: OpenCLAddressSpace,
                     tm: TypedOpenCLMemory): TypedOpenCLMemory = {
        addressSpace match {
          case GlobalMemory | PrivateMemory =>
            TypedOpenCLMemory(tm.mem, ArrayType(tm.t, Type.getMaxLength(t)))

          case LocalMemory =>
            m match {
              case _: MapGlb | _: MapWrg  | _: Map =>
                tm
              case _: MapLcl | _: MapWarp | _: MapLane | _: MapSeq =>
                TypedOpenCLMemory(tm.mem, ArrayType(tm.t, Type.getMaxLength(t)))
            }
          case coll: AddressSpaceCollection =>
            changeType(coll.findCommonAddressSpace(), tm)
        }
      }

      // change types for all of them
      val cts = mems.map( (tm: TypedOpenCLMemory) => changeType(tm.mem.addressSpace, tm) )

      // TODO: Think about other ways of refactoring this out 
      m match {
        case aw : MapAtomWrg => 
          cts :+ TypedOpenCLMemory(aw.globalTaskIndex, ArrayType(Int, Cst(1)))
        case _ => cts
      }
      
    }

    def collectReduce(r: AbstractPartRed,
                      argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
      val mems = collect(r.f.body)

      mems.filter(m => {
        val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
        val isAlreadyInParams =  params.exists(_.mem.variable == m.mem.variable)

        !isAlreadyInArgs && !isAlreadyInParams
      })
    }

    def collectSearch(s: AbstractSearch, call:FunCall, argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
      val mems = collect(s.f.body)

      // TODO: Optimise so we use the default value instead of more allocated memory!
      TypedOpenCLMemory(call) +: mems.filter(m => {
        val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
        val isAlreadyInParams =  params.exists(_.mem.variable == m.mem.variable)

        !isAlreadyInArgs && !isAlreadyInParams
      })
    }

    def collectIterate(call: FunCall, i: Iterate): Seq[TypedOpenCLMemory] = {
      TypedOpenCLMemory(i.swapBuffer, ArrayType(call.args.head.t, ?)) +: collect(i.f.body)
    }

    // this prevents that multiple memory objects (possibly with different types) are collected
    // multiple times
    def distinct(seq: Seq[TypedOpenCLMemory]) = {
      val b = Seq.newBuilder[TypedOpenCLMemory]
      val seen = scala.collection.mutable.HashSet[OpenCLMemory]()
      for (x <- seq) {
        if (!seen(x.mem)) {
          b += x
          seen += x.mem
        }
      }
      b.result()
    }

    // actual function impl
    params.map(TypedOpenCLMemory(_)) ++ distinct(collect(expr))
  }
}
