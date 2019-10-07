package utils

import ir._
import ir.view.{AccessVar, CastedPointer}
import lift.arithmetic._
import opencl.generator.{NotPrintableExpression, OclFunction}

// TODO: Move much of this code to the new generic.AstPrinter module
//       (UPD) Don't -- the generic.AstPrinter might be as inefficient as
//       PrettyPrinter with StackOverflows on long kernels due to the stateful approach
//       But do merge the two somehow.
// TODO: Place in a C-like backend-specific package

object Printer {

  def toString(t: Type, seenArray: Boolean = false): String = {
    t match {
      case ArrayType(elemT)       =>
        val s = toString(elemT, seenArray = true)
        if (!seenArray) s + "*" else s
      case VectorType(elemT, len) => toString(elemT, seenArray) + toString(len)
      case ScalarType(name, _)    => name
      case tt: TupleType          => Type.name(tt)
      case NoType                 => "void"
      case _                      => throw new NotPrintableExpression(t.toString)
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
      case of: OclFunction                         => of.toOCLString
      case AccessVar(array, idx, _, _)             => s"${toString(array)}[${toString(idx)}]"
      case CastedPointer(v, ty, ofs, addressSpace, _) =>
        val offset = if (ofs == Cst(0)) "" else s" + ${toString(ofs)}"
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

}
