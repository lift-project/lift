package rewriting.utils

import ir._
import ir.ast._
import ir.ast.debug.{AssertType, PrintType}
import lift.arithmetic._
import opencl.generator.NotPrintableExpression
import opencl.ir.ast.OpenCLBuiltInFun
import opencl.ir.pattern._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

case class ScalaPrinter(printNonFixedVarIds: Boolean = true) {
  def apply(expr: Expr): String = {
    expr match {
      case funCall: FunCall => funCall.f match {
        case AssertType(_, _, _) | PrintType(_, _) | RewritingGuidePost(_) =>
          // Skip these primitives and just print their arguments
          apply(funCall.args.head)
        case _ =>
          s"FunCall(${apply(funCall.f)}, ${funCall.args.map(apply).mkString(", ")})"
      }
      case value: Value => s"""Value("${value.value}", ${apply(value.t)})"""
      case param: Param => "p_" + param.hashCode()
      case _ => expr.toString
    }
  }

  def apply(idxFun: IndexFunction): String = {
    idxFun match {
      case ReorderWithStride(s) => f"ReorderWithStride(${apply(s)})"
      case x => throw new NotImplementedError(s"$x")
    }
  }

  def apply(funDecl: FunDecl): String = {
    funDecl match {
      case lambda: Let => s"new Let((${lambda.params.map(apply).mkString(", ")}) => ${apply(lambda.body)})"
      case lambda: Lambda => s"fun((${lambda.params.map(apply).mkString(", ")}) => ${apply(lambda.body)})"
      case map: Map => s"Map(${apply(map.f)})"
      case mapSeq: MapSeq => s"MapSeq(${apply(mapSeq.f)})"
      case mapSeqVector: MapSeqVector => s"MapSeqVector(${apply(mapSeqVector.fVectorized)}, " +
                                            s"${apply(mapSeqVector.fScalar)}, ${apply(mapSeqVector.vectorLen)})"
      case mapWrg: MapWrg => s"MapWrg(${mapWrg.dim})(${apply(mapWrg.f)})"
      case mapLcl: MapLcl => s"MapLcl(${mapLcl.dim})(${apply(mapLcl.f)})"
      case mapWrg: MapAtomWrg => s"MapAtomWrg(${mapWrg.dim})(${apply(mapWrg.f)})"
      case mapLcl: MapAtomLcl => s"MapAtomLcl(${mapLcl.dim})(${apply(mapLcl.f)})"
      case mapGlb: MapGlb => s"MapGlb(${mapGlb.dim})(${apply(mapGlb.f)})"
      case reduceSeqUnroll: ReduceSeqUnroll => s"ReduceSeqUnroll(${apply(reduceSeqUnroll.f)})"
      case reduceSeq: ReduceSeq => s"ReduceSeq(${apply(reduceSeq.f)})"
      case mapSeqSlide: MapSeqSlide => s"MapSeqSlide(${apply(mapSeqSlide.f)})"
      case reduce: Reduce => s"Reduce(${apply(reduce.f)})"
      case reduce: PartRed => s"PartRed(${apply(reduce.f)})"
      case slide: Slide => s"Slide(${apply(slide.size)}, ${apply(slide.step)})"
      case toGlobal: toGlobal => s"toGlobal(${apply(toGlobal.f)})"
      case toLocal: toLocal => s"toLocal(${apply(toLocal.f)})"
      case toPrivate: toPrivate => s"toPrivate(${apply(toPrivate.f)})"
      case bsearch: BSearch => s"BSearch(${apply(bsearch.f)})"
      case lsearch: LSearch => s"LSearch(${apply(lsearch.f)})"
      case vec: VectorizeUserFun => s"VectorizeUserFun(${apply(vec.n)},${vec.userFun})"
      case Zip(n) => s"Zip($n)"
      case Join() => s"Join()"
      case Split(n) => s"Split(${apply(n)})"
      case Gather(idxFun) => s"Gather(${apply(idxFun)})"
      case Scatter(idxFun) => s"Scatter(${apply(idxFun)})"
      case Barrier(l, g) => s"Barrier(local = $l, global = $g)"
//      case uf: UserFun => apply(uf)
      case x => x.toString//throw new NotImplementedError(s"$x") // it's not ideal. we fix it later (c) Christophe
    }
  }

  def apply(arithExpr: ArithExpr): String = ArithExpr.printToScalaString(arithExpr, printNonFixedVarIds)

  def apply(t: Type): String = {
    t match {
      case opencl.ir.Float => "Float"
      case opencl.ir.Int => "Int"
      case opencl.ir.Double => "Double"
      case opencl.ir.Bool => "Bool"
      case TupleType(tt@_*) => s"TupleType(${tt.map(apply).mkString(", ")})"
      case VectorType(elemT, len) => s"VectorType(${apply(elemT)}, ${apply(len)})"
      case ArrayTypeWSWC(elemT, s, c) if s == c => s"ArrayType(${apply(elemT)}, ${apply(s)})"
      case ArrayTypeWSWC(elemT, s, c) => s"ArrayTypeWSWC(${apply(elemT)}, ${apply(s)}, ${apply(c)})"

      case NoType =>
        throw new NotPrintableExpression(s"Can not print NoType")
      case UndefType =>
        throw new NotPrintableExpression(s"Can not print UndefType")
      case s: ScalarType =>
        throw new NotPrintableExpression(s"Can not print ScalarType: $s")
      case unknown =>
        throw new NotPrintableExpression(s"Can not print: $unknown")
    }
  }

  def apply(uf: UserFun): String = {
    val name = "\"" + uf.name + "\""
    val paramNames = uf.paramNames.map("\"" + _ + "\"").mkString(", ")
    val inTs = uf.inTs.map(apply).mkString(", ")
    val outT = apply(uf.outT)

    val body = "\"\"\"" +
      uf.body.split("\n").map("|" + _).mkString("\n") +
      "\"\"\".stripMargin"

    s"val ${uf.name} = UserFun($name, Array($paramNames), $body, Seq($inTs), $outT)"
  }

  def apply(uf: OpenCLBuiltInFun): String = ""
}
