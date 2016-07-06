package openmp.generator

import apart.arithmetic.SizeVar
import openmp.executor.Compile
import c.generator.CAst.Block
import c.generator.{CAst, CGenerator}
import ir.{ArrayType, TupleType}
import ir.ast.{Expr, FunCall, fun}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import openmp.ir.pattern.{MapPar, MapVec, OmpMap}

/**
  * Created by Federico on 30-Jun-16.
  */
object OMPGenerator extends CGenerator{
  override protected def generateExprPostArguments(expr: Expr, block: Block): Unit = {
      expr match {
        case call:FunCall => call.f match {
          case m:MapPar => generateOmpMapCall("omp parallel for",m,call,block)
          case m:MapVec => generateOmpMapCall("omp parallel for simd",m,call,block)
          case _ => super.generateExprPostArguments(expr, block)
        }
        case _ => super.generateExprPostArguments(expr, block)
      }
  }

  // MapPar
  private def generateOmpMapCall(pragma:String,
                                 m: OmpMap,
                                 call: FunCall,
                                 block: Block): Unit = {
    (block: Block) += CAst.Comment("omp_map")
    (block: Block) += CAst.Pragma(pragma)
    generateForLoop(block, m.loopVar, generateExpr(m.f.body, _), m.shouldUnroll)
    (block: Block) += CAst.Comment("end omp_map")
  }
}

object Test {
  def main(args:Array[String]):Unit = {
    val f = fun(
      ArrayType(TupleType(Float,Float),100),
      A => {
        MapPar(add) $ A
      })
    val f2 = fun (
      ArrayType(Float, SizeVar("N")),
      Float,
      (in,init) => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, init) $ in
      })
    println(Compile(f))
  }
}
