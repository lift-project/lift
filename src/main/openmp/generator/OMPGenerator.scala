package openmp.generator

import apart.arithmetic.{Cst, _}
import openmp.executor.Compile
import c.generator.CAst.{ArithExpression, AssignmentExpression, Block, CondExpression, ExpressionStatement, VarDecl}
import c.generator.{CAst, CGenerator}
import ir.{ArrayType, TupleType}
import ir.ast.{Expr, FunCall, fun}
import opencl.generator.{OclFunction, OpenCLGeneratorException}
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
    generateOpenMPLoop(block, m.loopVar, generateExpr(m.f.body, _), m.shouldUnroll)
    (block: Block) += CAst.Comment("end omp_map")
  }

  protected def generateOpenMPLoop(block: Block,
                                indexVar: Var,
                                generateBody: (Block) => Unit,
                                needUnroll: Boolean = false): Unit = {

    val range = indexVar.range.asInstanceOf[RangeAdd]
    val step = range.step
    val init = ArithExpression(range.start)
    val stop = range match {
      case ra: RangeAdd => ra.stop
      case _ => throw new OpenCLGeneratorException("Cannot handle range for ForLoop: " + range)
    }
    val cond = CondExpression(ArithExpression(indexVar), ArithExpression(stop), CondExpression.Operator.<)

    // if we need to unroll (e.g. because of access to private memory)
    if (needUnroll) {
      val iterationCount = try {
        indexVar.range.numVals.eval
      } catch {
        case _: NotEvaluableException =>
          throw new OpenCLGeneratorException("Trying to unroll loop, but iteration count " +
            "could not be determined statically.")
      }

      if (iterationCount > 0) {
        (block: Block) += CAst.Comment("unroll")

        for (i <- 0 until iterationCount) {
          replacements = replacements.updated(indexVar, i)
          val j: ArithExpr =
            if (range.min.isInstanceOf[OclFunction]) {
              range.min + step * i
            } else {
              i
            }
          replacementsWithFuns = replacementsWithFuns.updated(indexVar, j)

          generateBody(block)
        }
        // cleanup
        replacements = replacements - indexVar
        replacementsWithFuns = replacementsWithFuns - indexVar

        (block: Block) += CAst.Comment("end unroll")
        return
      } else {
        throw new OpenCLGeneratorException(s"Trying to unroll loop, but iteration count is $iterationCount.")
      }

    }

    // TODO: Information needed elsewhere. See analysis.ControlFlow
    // try to see if we really need a loop
    //Optimized introduced in order to stop messing with OpenMP loops!!
      indexVar.range.numVals match {
        case Cst(0) =>
          // zero iterations
          (block: Block) += CAst.Comment("iteration count is 0, no loop emitted")
          return

        case Cst(1) =>
          generateStatement(block, indexVar, generateBody, init)
          return

        // TODO: See TestInject.injectExactlyOneIterationVariable
        // TODO: M / 128 is not equal to M /^ 128 even though they print to the same C code
        case _ if range.start.min.min == Cst(0) &&
          range.stop.substituteDiv == range.step.substituteDiv =>

          generateStatement(block, indexVar, generateBody, init)
          return

        // TODO: See TestOclFunction.numValues and issue #62
        case _ if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
          generateIfStatement(block, indexVar, generateBody, init, stop)
          return
        case _ =>
          (indexVar.range.numVals.min, indexVar.range.numVals.max) match {
            case (Cst(0), Cst(1)) =>
              // one or less iteration
              generateIfStatement(block, indexVar, generateBody, init, stop)
              return

            case _ =>
      }
    }

    val increment = AssignmentExpression(ArithExpression(indexVar), ArithExpression(indexVar + range.step))
    val innerBlock = CAst.Block(Vector.empty)
    (block: Block) += CAst.ForLoop(VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory), ExpressionStatement(cond), increment, innerBlock)
    generateBody(innerBlock)
  }
}
