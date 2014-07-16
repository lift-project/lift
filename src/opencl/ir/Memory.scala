package opencl.ir

import scala.collection.mutable

import ir._
import ir.Fun

abstract class OpenCLAddressSpace

object LocalMemory extends OpenCLAddressSpace {
  override def toString = "local"
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString = "global"
}

object UndefAddressSpace extends OpenCLAddressSpace

case class OpenCLMemory(variable: Var, size: Expr, t: Type, addressSpace: OpenCLAddressSpace) extends Memory {

  if (!t.isInstanceOf[ArrayType])
    throw new IllegalArgumentException

  if (TypeVar.getTypeVars(size).nonEmpty)
    throw new IllegalArgumentException

  override def toString = "(" + variable + ", " + ExprSimplifier.simplify(size) + ", " + t + ", " + addressSpace + ")"
}

class OpenCLSubMemory(parent: OpenCLMemory, size: Expr, t: Type, accessFun: (Expr) => Expr) extends OpenCLMemory(parent.variable, size, t, parent.addressSpace)

// Allocate memory globally
class GlobalAllocator {


}

object OpenCLNullMemory extends OpenCLMemory(Var("NULL"), Cst(0), ArrayType(UndefType,?), UndefAddressSpace)

object OpenCLMemory {

  def getMaxSizeInBytes(t: Type) : Expr = {
    Expr.max(getSizeInBytes(t))
  }

  private def getSizeInBytes(t: Type) : Expr = {
    ExprSimplifier.simplify(
      t match {
        case st: ScalarType => st.size
        case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
        case at: ArrayType => at.len * getSizeInBytes(at.elemT)
        case tt: TupleType => tt.elemsT.map(getSizeInBytes).reduce(_ + _)
        case _ => throw new TypeException(t, "??")
      }
    )
  }

  def asOpenCLMemory(m : Memory) : OpenCLMemory = {
    m match {
      case oclm : OpenCLMemory => oclm
      case UnallocatedMemory => OpenCLNullMemory
      case _ => throw new IllegalArgumentException
    }
  }


  // allocate input memory if needed (should only be for top-level function)
  def fixInput(f: Fun, inputMem : OpenCLMemory) : OpenCLMemory = {
    if (inputMem == OpenCLNullMemory) {
      f match {
        case _: Input => OpenCLNullMemory
        case _: CompFun => OpenCLNullMemory
        case _ => OpenCLMemory(Var(ContinousRange(Cst(0), getMaxSizeInBytes(f.inT))), getMaxSizeInBytes(f.inT), f.inT, GlobalMemory)
      }
    }
    else
      inputMem
  }

  // allocate an output memory based on the input memory
  def allocOutput(glbOutSize: Expr, lclOutSize: Expr, inputMem: OpenCLMemory, ouputType: Type) : OpenCLMemory = {
    assert (inputMem != OpenCLNullMemory)

    inputMem.addressSpace match {
      case GlobalMemory => OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, ouputType, GlobalMemory)
      case LocalMemory => OpenCLMemory(Var(ContinousRange(Cst(0), lclOutSize)), lclOutSize, ouputType, LocalMemory)
    }
  }

  def alloc(f: Fun, numGlb: Expr = 1, numLcl: Expr = 0, argInMem : OpenCLMemory = OpenCLNullMemory, outputMem : OpenCLMemory = OpenCLNullMemory) : OpenCLMemory = {

    // fix in the input memory if needed
    val inMem = fixInput(f, argInMem)
    assert(inMem != OpenCLNullMemory || f.isInstanceOf[Input] || f.isInstanceOf[CompFun])
    f.inM = inMem

    val maxGlbOutSize = getMaxSizeInBytes(f.ouT) * numGlb
    val maxLclOutSize = getMaxSizeInBytes(f.ouT) * numLcl

    val maxLen = Expr.max(Type.getLength(f.ouT))
    val result : OpenCLMemory = f match {

      case Input(_,_) =>
        assert(outputMem == OpenCLNullMemory)
        OpenCLMemory(Var(ContinousRange(Cst(0), maxGlbOutSize)), maxGlbOutSize, f.ouT, GlobalMemory)

      case MapGlb(_) | MapWrg(_) => alloc(f.asInstanceOf[AbstractMap].f, numGlb * maxLen, numLcl, inMem, outputMem)
      case MapLcl(_) | MapWarp(_) | MapLane(_) => alloc(f.asInstanceOf[AbstractMap].f, numGlb * maxLen, numLcl * maxLen, inMem, outputMem)

      case tg: toGlobal =>
        val mem =
          if (outputMem == OpenCLNullMemory || outputMem.addressSpace != GlobalMemory)
            OpenCLMemory(Var(ContinousRange(Cst(0), maxGlbOutSize)), maxGlbOutSize, f.ouT, GlobalMemory)
        else
            outputMem
        alloc(tg.f, numGlb, numLcl, inMem, mem)

      case tl: toLocal =>
        val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != LocalMemory)
          OpenCLMemory(Var(ContinousRange(Cst(0), maxLclOutSize)), maxLclOutSize, f.ouT, LocalMemory)
        else
          outputMem
        alloc(tl.f, numGlb, numLcl, inMem, mem)

      case cf: CompFun =>
        cf.funs.foldRight(inMem)((f,mem) => alloc(f, numGlb, numLcl, mem))

      case it: Iterate =>

        val inSize = getMaxSizeInBytes(it.inT)
        val outSize = getMaxSizeInBytes(it.ouT)
        val largestSize = Expr.max(inSize, outSize)

        // create a swap buffer
        it.swapBuffer = OpenCLMemory(Var(ContinousRange(Cst(0), largestSize)), largestSize, ArrayType(it.inT,?), inMem.addressSpace)

        alloc(it.f, numGlb, numLcl, inMem)

      case fp: FPattern => alloc(fp.f, numGlb, numLcl, inMem, outputMem)

      // some function do not create anything, TODO: add toVector, ToScalar, reorder,...
      case Split(_) | Join() => inMem

      case _ =>
        if (outputMem == OpenCLNullMemory)
          f.ouT match {
            // TODO: could maybe allocated in private memory (need to change slightly the allocator and add toPrivate)
            case ScalarType(_,_) | VectorType(_,_) => allocOutput(maxGlbOutSize, maxLclOutSize, inMem, ArrayType(f.ouT, ?))
            case _ => allocOutput(maxGlbOutSize, maxLclOutSize, inMem, f.ouT)
          }

        else
          outputMem
    }

    assert (result != OpenCLNullMemory)
    f.outM = result

    result

  }


  def getAllocatedMemory(f: Fun) : Array[OpenCLMemory] = {
    val result = Fun.visit(Array[OpenCLMemory]())(f, (f,arr) => f match {
      case it : Iterate => arr :+ OpenCLMemory.asOpenCLMemory(f.inM) :+ OpenCLMemory.asOpenCLMemory(f.outM) :+ OpenCLMemory.asOpenCLMemory(it.swapBuffer)
      case _ => arr :+ OpenCLMemory.asOpenCLMemory(f.inM) :+ OpenCLMemory.asOpenCLMemory(f.outM)
    })

    val seen = mutable.HashSet[OpenCLMemory]()
    result.

      // remove null memory and duplicates while preserving the order
      foldLeft(Array[OpenCLMemory]())((arr, m) =>
      if (seen.contains(m) || m == OpenCLNullMemory)
        arr
       else {
        seen += m
        arr :+ m
      })

  }
  
}
