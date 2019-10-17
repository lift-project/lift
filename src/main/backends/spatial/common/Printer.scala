package backends.spatial.common

import _root_.ir.Type
import _root_.ir.{ArrayType, ScalarType}
import backends.spatial.accel.generator.NotPrintableExpression
import backends.spatial.common.ir.{DRAMMemory, RegMemory, SRAMMemory, SpatialAddressSpace}
import ir.view.{AccessVar, ArrayAddressor, CastedPointer, Index, Slice}
import lift.arithmetic.{ArithExpr, BitwiseAND, BitwiseXOR, Cst, IntDiv, LShift, Log, Lookup, Mod, Pow, Predicate, Prod, Sum, Var}

object Printer {
  def toString(addressSpace: SpatialAddressSpace): String = {
    addressSpace match {
      case DRAMMemory     => "DRAM"
      case SRAMMemory     => "SRAM"
      case RegMemory      => "Reg"
      case _          => throw new IllegalArgumentException("Unknown Spatial address space encountered during printing")
    }
  }

  def toString(t: Type): String = {
    t match {
      case ScalarType(name, _) => name
      case _                   => throw new IllegalArgumentException("Expected a scalar type during printing")
    }
  }

  def toString(t: Type, addressSpace: SpatialAddressSpace): String = {
    t match {
      case _: ScalarType  => toString(t)
      case at: ArrayType  => val dimensions = Type.getLengths(at).dropRight(1)
                             toString(addressSpace) + dimensions.length.toString +
                               "[" + Type.getBaseType(at) + "]"
      case _              => throw new IllegalArgumentException("Expected an array type during printing")
    }
  }

  def toString(e: ArithExpr): String = {
    e match {
      case Cst(c)                                  => c.toString
      case Pow(b, ex)                              =>
        "(int)pow((float)" + toString(b) + ", " + toString(ex) + ")"
      case Log(b, x)                               => "(int)log" + b + "((float)" + toString(x) + ")"
      case Prod(es)                                =>
        val (denTerms, numTerms) = es.partition({
          case Pow(_, Cst(-1)) => true
          case _               => false
        })
        val num = toString(numTerms)
        if (denTerms.isEmpty) s"($num)"
        else {
          val den = toString(denTerms.map({
            case Pow(x, Cst(-1)) => x
            case _               => throw new IllegalArgumentException()
          }))
          s"(($num)/($den))"
        }
      case Sum(es)                                 => "(" + es.map(toString).reduce(_ + " + " + _) + ")"
      case Mod(a, n)                               => "(" + toString(a) + " % " + toString(n) + ")"
      case AccessVar(array, idxs, _, _)            => s"${toString(array)}(" + idxs.foldLeft("")(_ + toString(_)) + "]"
      case CastedPointer(v, ty, ofs, addressSpace, _) =>
        val offset = if (ofs.ae == Cst(0)) "" else s" + ${toString(ofs)}"
        s"(($addressSpace ${Type.name(ty)}*)(${toString(v)}$offset))"
      case v: Var                                  => v.toString
      case IntDiv(n, d)                            => "(" + toString(n) + " / " + toString(d) + ")"
      case lu: Lookup                              => "lookup" + lu.id + "(" + toString(lu.index) + ")"
      case BitwiseXOR(a, b)                        => "(" + toString(a) + "^" + toString(b) + ")"
      case BitwiseAND(a, b)                        => "(" + toString(a) + "&" + toString(b) + ")"
      case LShift(a, b)                            => "(" + toString(a) + " << " + toString(b) + ")"
      case i: lift.arithmetic.IfThenElse           =>
        s"( (${toString(i.test.lhs)} ${i.test.op} ${toString(i.test.rhs)}) ? " +
          s"${toString(i.t)} : ${toString(i.e)} )"
      case _                                       => throw new NotPrintableExpression(e.toString)
    }
  }

  def toString(terms: Seq[ArithExpr]): String = {
    val res = terms.foldLeft("1")((s, e) => s + " * " + toString(e))
    if (terms.isEmpty) "1"
    else res.drop(4) // Drop "1 * "
  }

  def toString(p: Predicate): String = {
    s"(${toString(p.lhs)} ${p.op} ${toString(p.rhs)})"
  }

  def toString(addr: ArrayAddressor): String= {
    addr match {
      case Index(idx)               => toString(idx)
      case Slice(start, step, end)  => toString(start) + " :: " + toString(step) + " :: " + toString(end)
      case _                        => throw new NotPrintableExpression(addr.toString)
    }
  }
}
