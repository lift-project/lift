package opencl.ir

import lift.arithmetic._
import arithmetic.TypeVar
import ir._
import ir.ast._
import opencl.ir.pattern._


private class MemoryAllocationException(msg: String)
  extends IllegalArgumentException(msg)

/** Represents memory in OpenCL as a raw collection of bytes allocated in an
  * OpenCL address space.
  *
  * @constructor Create a new OpenCLMemory object
  * @param variable The variable associated with the memory
  * @param size The size of the memory as numbers bytes
  * @param addressSpace The address space where the memory has been allocated
  */
sealed class OpenCLMemory(var variable: Var,
                   val size: ArithExpr,
                   val addressSpace: OpenCLAddressSpace) extends Memory {

  // size cannot be 0 unless it is the null memory
  try {
    if (size.eval == 0)
      throw new MemoryAllocationException("Cannot have a memory of 0 bytes!")
  } catch {
    case NotEvaluableException() => // nothing to do
    case e: Exception => throw e
  }

  // no type variable allowed in the size
  if (TypeVar.getTypeVars(size).nonEmpty)
    throw new MemoryAllocationException("Cannot allocate memory for abstract types")

  // no unknown allowed in the size
  // val hasUnknown = ArithExpr.visitUntil(size, _ == ?)
  // TODO: think about this
  // if (hasUnknown)
  //   throw new IllegalArgumentException



  def copy(): OpenCLMemory = {
    addressSpace match {
      case GlobalMemory => OpenCLMemory.allocGlobalMemory(size)
      case LocalMemory => OpenCLMemory.allocLocalMemory(size)
      case PrivateMemory => OpenCLMemory.allocPrivateMemory(size)
      case AddressSpaceCollection(_) => this match {
        case coll: OpenCLMemoryCollection =>
          OpenCLMemoryCollection(coll.subMemories.map(_.copy()))
        case _ => throw new IllegalArgumentException()
      }
      case UndefAddressSpace => this
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

case class OpenCLMemoryCollection(subMemories: Array[OpenCLMemory],
                             override val addressSpace: AddressSpaceCollection)
  extends OpenCLMemory(Var("Tuple"), subMemories.map(_.size).reduce(_+_),
                       addressSpace)

object OpenCLMemoryCollection {
  def apply(mems: Seq[OpenCLMemory]): OpenCLMemoryCollection = {
    val addressSpace = AddressSpaceCollection(mems.map(_.addressSpace))
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
  @scala.annotation.tailrec
  def allocMemory(glbOutSize: ArithExpr,
                  lclOutSize: ArithExpr,
                  pvtOutSize: ArithExpr,
                  addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    if (addressSpace == UndefAddressSpace)
      throw new IllegalArgumentException(s"Can't allocate memory in $addressSpace")

    addressSpace match {
      case GlobalMemory => allocGlobalMemory(glbOutSize)
      case LocalMemory => allocLocalMemory(lclOutSize)
      case PrivateMemory => allocPrivateMemory(pvtOutSize)
      case co: AddressSpaceCollection =>
        allocMemory(glbOutSize, lclOutSize, pvtOutSize, co.findCommonAddressSpace())
      case UndefAddressSpace =>
        throw new MemoryAllocationException("Cannot allocate memory in UndefAddressSpace")
    }
  }

  /**
    * Return newly allocated memory of `size` bytes in `addressSpace`
    *
    * @param size Size of the memory to allocate in bytes
    * @param addressSpace Address space for the allocated memory
    */
  def allocMemory(size: ArithExpr, addressSpace: OpenCLAddressSpace) =
    OpenCLMemory(Var("", ContinuousRange(Cst(0), size)), size, addressSpace)

  /** Return newly allocated global memory */
  def allocGlobalMemory(glbOutSize: ArithExpr): OpenCLMemory =
    allocMemory(glbOutSize, GlobalMemory)

  /** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: ArithExpr): OpenCLMemory =
    allocMemory(lclOutSize, LocalMemory)

  def allocPrivateMemory(size: ArithExpr): OpenCLMemory =
    allocMemory(size, PrivateMemory)
}

/** Represents an OpenCLMemory object combined with a type.
  *
  * @constructor Create a new TypedOpenCLMemory object
  * @param mem The underlying memory object
  * @param t The type associated with the memory object
  */
case class TypedOpenCLMemory(mem: OpenCLMemory, t: Type) {
  override def toString: String = s"($mem: $t)"
}

object TypedOpenCLMemory {
  def apply(expr: Expr): TypedOpenCLMemory = {
    new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(expr.mem), expr.t)
  }

  def apply(mem: Memory, t: Type): TypedOpenCLMemory = {
    new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(mem), t)
  }

  /**
    * Function to collect typed OpenCL memory objects of the given lambda.
    * Returns a tuple with three components:
    * the first component are the memory objects for the given parameters -- in the same order;
    * the second component are output memory objects;
    * the third component are temporary memory objects.
    *
    * @param f The lambda from which memory objects are collected.
    * @param includePrivate indicate if private memories should be included in the intermediates.
    * @return A tuple with the memory objects of the (inputs, intermediates, outputs).
    */
  def collect(f: Lambda,
              includePrivate: Boolean = false):
  (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory]) = {

    def collectIntermediateMemories(expr: Expr): Seq[TypedOpenCLMemory] = {
      expr match {
        case v: Value =>
          if (includePrivate) {
            Seq(TypedOpenCLMemory(v))
          } else {
            Seq()
          }
        case _: Param => Seq()
        case _: ArrayConstructors => Seq()
        case call: FunCall => collectFunCall(call)
      }
    }

    def collectFunCall(call: FunCall): Seq[TypedOpenCLMemory] = {
      val argMemories: Seq[TypedOpenCLMemory] = call.args.length match {
        case 0 => Seq()
        case 1 => collectIntermediateMemories(call.args.head)
        case _ => call.args.map(collectIntermediateMemories).reduce(_ ++ _)
      }

      val bodyMemories = call.f match {
        case _: UserFun | _: VectorizeUserFun => collectUserFun(call)
        case l: Lambda              => collectIntermediateMemories(l.body)
        case m: AbstractMap         => collectMap(call.t, m)
        case f: FilterSeq           => collectIntermediateMemories(f.f.body) :+ TypedOpenCLMemory(call)
        case r: AbstractPartRed     => collectReduce(r, argMemories)
        case sp: MapSeqSlide        => collectMapSeqSlide(sp, argMemories)
        case s: AbstractSearch      => collectSearch(s, call, argMemories)
        case _: UnsafeArrayAccess   => Seq(TypedOpenCLMemory(call))
        case _: CheckedArrayAccess  => Seq(TypedOpenCLMemory(call))
        case i: Iterate             => collectIterate(call, i)
        case fp: FPattern           => collectIntermediateMemories(fp.f.body)
        case _                      => Seq()
      }

      argMemories ++ bodyMemories
    }

    def collectUserFun(call: FunCall): Seq[TypedOpenCLMemory] = call.mem match {
      case m: OpenCLMemory =>
        if (!includePrivate && m.addressSpace == PrivateMemory) {
          Seq()
        } else {
          Seq(TypedOpenCLMemory(call))
        }
    }

    def collectMap(t: Type,
                   m: AbstractMap): Seq[TypedOpenCLMemory] = {
      val mems = collectIntermediateMemories(m.f.body)

      @scala.annotation.tailrec
      def changeType(addressSpace: OpenCLAddressSpace,
                     tm: TypedOpenCLMemory): TypedOpenCLMemory = {
        // TODO: This might return one of two types in case of reduce (T or Array(T, 1))
        addressSpace match {
          case PrivateMemory =>
            m match {
              case _: MapGlb | _: MapWrg  | _: Map =>
                tm
              case _: MapLcl | _: MapWarp | _: MapLane | _: MapSeq =>

                val privateMultiplier = if (m.iterationCount == ?) Cst(1) else m.iterationCount

                TypedOpenCLMemory(tm.mem, ArrayTypeWSWC(tm.t, privateMultiplier))
            }
          case LocalMemory =>
            m match {
              case _: MapGlb | _: MapWrg  | _: Map =>
                tm
              case _: MapLcl | _: MapWarp | _: MapLane | _: MapSeq =>
                val newType = t.asInstanceOf[ArrayType].replacedElemT(tm.t)
                TypedOpenCLMemory(tm.mem, newType)
            }
          case GlobalMemory =>
            val newType = t.asInstanceOf[ArrayType].replacedElemT(tm.t)
            TypedOpenCLMemory(tm.mem, newType)

          case coll: AddressSpaceCollection =>
            changeType(coll.findCommonAddressSpace(), tm)

          case UndefAddressSpace =>
            throw new MemoryAllocationException("Address space must be known at this point")
        }
      }

      // change types for all of them
      val cts = mems.map( (tm: TypedOpenCLMemory) => changeType(tm.mem.addressSpace, tm) )

      // TODO: Think about other ways of refactoring this out
      m match {
        case aw : MapAtomWrg =>
          cts :+ TypedOpenCLMemory(aw.globalTaskIndex, ArrayTypeWSWC(Int, Cst(1)))
        case _ => cts
      }

    }

    def collectReduce(r: AbstractPartRed,
                      argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
      val mems: Seq[TypedOpenCLMemory] = collectIntermediateMemories(r.f.body) ++ (r match {
        case rws: ReduceWhileSeq => collectIntermediateMemories(rws.p.body)
        case _ => Seq[TypedOpenCLMemory]()
      })

      mems.filter(m => {
        val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
        val isAlreadyInParams =  f.params.exists(_.mem.variable == m.mem.variable)

        !isAlreadyInArgs && !isAlreadyInParams
      })
    }

    def collectMapSeqSlide(sp: MapSeqSlide,
                           argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
      val mems: Seq[TypedOpenCLMemory] = collectIntermediateMemories(sp.f.body) ++ Seq[TypedOpenCLMemory]()

      mems.filter(m => {
        val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
        val isAlreadyInParams =  f.params.exists(_.mem.variable == m.mem.variable)

        !isAlreadyInArgs && !isAlreadyInParams
      })
    }

    def collectSearch(s: AbstractSearch, call:FunCall, argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
      val mems = collectIntermediateMemories(s.f.body)

      // TODO: Optimise so we use the default value instead of more allocated memory!
      TypedOpenCLMemory(call) +: mems.filter(m => {
        val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
        val isAlreadyInParams =  f.params.exists(_.mem.variable == m.mem.variable)

        !isAlreadyInArgs && !isAlreadyInParams
      })
    }

    def collectIterate(call: FunCall, i: Iterate): Seq[TypedOpenCLMemory] = {
      i.swapBuffer match {
        case UnallocatedMemory => collectIntermediateMemories(i.f.body)
        case _ =>
          TypedOpenCLMemory(i.swapBuffer, ArrayTypeWSWC(call.args.head.t, ?)) +: collectIntermediateMemories(i.f.body)
      }
    }

    // this prevents that multiple memory objects (possibly with different types) are collected
    // multiple times without changing the order
    def distinct(seq: Seq[TypedOpenCLMemory]) = {
      val builder = Seq.newBuilder[TypedOpenCLMemory]
      val seen = scala.collection.mutable.HashSet[OpenCLMemory]()
      for (x <- seq) {
        if (!seen(x.mem)) {
          builder += x
          seen += x.mem
        }
      }
      builder.result()
    }

    val inputs = f.params.map(TypedOpenCLMemory(_))
    val output = TypedOpenCLMemory(f.body)

    val intermediates =
      distinct(collectIntermediateMemories(f.body)).filter(_.mem != output.mem)

    val (localIntermediates, globalIntermediates) =
      intermediates.partition(_.mem.addressSpace == LocalMemory)

    (inputs, Seq(output), globalIntermediates, localIntermediates)
  }

  /**
    * Function to collect typed OpenCL memory objects of the given lambda.
    * Returns a flat sequence with the memory objects in the order: [inputs ++ outputs ++ intermediates]
    *
    * @param f The lambda from which memory objects are collected.
    * @param includePrivate indicate if private memories should be included in the intermediates.
    * @return A flat sequence with the memory objects in the order: [inputs ++ outputs ++ intermediates]
    */
  def collectAsFlatSequence(f: Lambda, includePrivate: Boolean = false): Seq[TypedOpenCLMemory] = {
    val (inputs, outputs, globalIntermediates, localIntermediates) = collect(f, includePrivate)
    inputs ++ outputs ++ globalIntermediates ++ localIntermediates
  }
}
