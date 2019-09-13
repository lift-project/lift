package backends.c.common.utils.output_view

import ir.ast.{Expr, Lambda, Param}
import ir.view.{NoView, UnusedInExprOutputView, ViewMem, ViewNull}
import backends.c.common.utils.common_view.GenerateViewForRawInOut.generateViewForRawInOut2
import lift.arithmetic.Cst

import scala.collection.mutable

object OutputView {

  def pre_check(lambda: Lambda) : Unit = {

    lambda visitBy {
      case e:Expr => e.outputView = NoView
      case _ =>
    }

    lambda visitBy {
      case e:Expr => assert(e.outputView == NoView)
      case _ =>
    }

  }

  def post_check(lambda: Lambda) : Unit = {

    //If some params are not used in expression,
    //set their outputView explicitly to avoid NoView assertion failure
    val all_params = lambda.params.toSet
    val used_params = mutable.Set.empty[Param]
    lambda.body visitBy {
      case p:Param if all_params contains p => used_params += p
      case _ =>
    }
    val used_params_immutable = used_params.toSet
    val unused_params = all_params -- used_params_immutable
    unused_params.foreach(p => p.outputView = UnusedInExprOutputView)

    lambda visitBy {
      case e:Expr =>
        assert( e.outputView != NoView )
        //ViewNull should not occur in the outputView after pass
        e.outputView match {
          case ViewNull() => assert(false)
          case _ =>
        }
      case _ =>
    }

  }

  def init_body(lambda: Lambda) : Unit = {

    //first set the body's output view, then propagate to someone inside.
    //lambda.body.outputView = ViewMem(lambda.body.mem.variable, lambda.body.t)

    //lambda.body.outputView = generateViewForRawInOut(lambda.body, lambda.body.t, Cst(1), outputViewConstruct = true)
    lambda.body.outputView = generateViewForRawInOut2(lambda.body, lambda.body.t, Cst(1))

  }

}
