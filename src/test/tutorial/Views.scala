package tutorial

import ir._
import ir.ast._
import ir.printer.DotPrinter
import ir.view.InputView
import lift.arithmetic._
import opencl.executor.{Compile, Execute}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite
import rewriting.macrorules.EnablingRules
import rewriting.rules.{FusionRules, Rules}


class Views{
  /**
    * In the Lift IR, arrays are not accessed explicitly but implicitly;
    * the patterns determine which thread accesses which element in memory.
    * This design simplifies the process of lowering high-level programs to
    * the Lift IR and guarantees that data races are avoided by construction
    * since no arbitrary accesses into memory are permitted. However, this
    * introduces two main challenges when compiling the Lift IR: First,
    * avoiding unnecessary intermediate results arising from function which
    * change  only  the  data  layout;  And,  secondly,  generating
    * efficient accesses to multi-dimensional arrays which have a flat
    * representation in memory.
    */

  @Test
  def views(): Unit = {
    val simpleProgram = \(
      ArrayType(Float, SizeVar("N")), input =>
        MapGlb(id) $ input
    )
    // OUT[global_id] = id(IN[global_id]);

    //val simpleKernel = Compile(simpleProgram)
    //println(simpleKernel)

    val tiledProgram = \(
      ArrayType(Float, SizeVar("N")), input =>
        Join() o MapGlb(MapSeq(id)) o Split(4) $ input
    )
    // OUT[(i + (4 * global_id))] = id(OUT[(i + (4 * global_id))]);

    val tiledKernel = Compile(tiledProgram)
    //DotPrinter("/home/bastian/development/tutorial", "expr", tiledProgram)
    //println(tiledKernel)

    // view construction
    //                                  <- dataflow-wise
    // Join() o MapGlb(MapSeq(id)) o Split(4) $ input
    InputView(tiledProgram.body)

  }

}
