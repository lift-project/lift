package openmp.generator

import openmp.executor.Compile
import c.generator.CAst.Block
import c.generator.{CAst, CGenerator}
import ir.{ArrayType, TupleType}
import ir.ast.{Expr, FunCall, fun}
import opencl.ir._
import openmp.ir.pattern.MapPar

/**
  * Created by Federico on 30-Jun-16.
  */
object OMPGenerator extends CGenerator{
  override protected def generateExprPostArguments(expr: Expr, block: Block): Unit = {
      expr match {
        case call:FunCall => call.f match {
          case m:MapPar => generateMapParCall(m,call,block)
          case _ => super.generateExprPostArguments(expr, block)
        }
        case _ => super.generateExprPostArguments(expr, block)
      }
  }

  // MapSeq
  private def generateMapParCall(m: MapPar,
                                 call: FunCall,
                                 block: Block): Unit = {
    (block: Block) += CAst.Comment("map_par")
    (block: Block) += CAst.Pragma("omp parallel for")
    generateForLoop(block, m.loopVar, generateExpr(m.f.body, _), m.shouldUnroll)
    (block: Block) += CAst.Comment("end map_par")
  }
}

object Test {
  def main(args:Array[String]):Unit = {
    val f = fun(
      ArrayType(TupleType(Float,Float),100),
      A => {
        MapPar(add) $ A
      })
    println(Compile(f))
  }
}
