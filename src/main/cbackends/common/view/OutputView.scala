package cbackends.common.view

import ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC}
import ir.ast.{AbstractMap, AbstractPartRed, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Expr, FunCall, Get, IRNode, Join, Lambda, Param, Split, TransposeW, UserFun, Value, Zip, transpose}
import ir.view._
import lift.arithmetic.{ArithExpr, Cst}
import core.generator.PrettyPrinter._

import scala.collection.mutable

object OutputView {

  def generateOutputView(node: IRNode): Unit = {
    node match {


      case a@ArrayFromUserFunGenerator(f, at) =>
        a.outputView = ViewGeneratorUserFun(f, at)
      case a@Array2DFromUserFunGenerator(f, at) =>
        a.outputView = View2DGeneratorUserFun(f, at)
      case a@Array3DFromUserFunGenerator(f, at) =>
        a.outputView = View3DGeneratorUserFun(f, at)

      case fc@FunCall(_:Zip, args@_*) => {

        assert(fc.outputView != NoView)

        val result = fc.outputView.unzip()

        args.zipWithIndex.foreach({
          //case (arg: Param, id) if arg.outputView == NoView =>
          case (arg: Expr, id) if arg.outputView == NoView =>
            arg.outputView = result.get(id)
          case _ =>
        })

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach(generateOutputView(_))


      }

      case fc@FunCall(Get(n), arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        generateOutputView(arg)
      }


      case fc@FunCall(Split(n), arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView.join(n);

        assert(arg.outputView != NoView)

        generateOutputView(arg)
      }
      case fc@FunCall(_:Join, arg) => {

        assert(fc.outputView != NoView)


        val n = fc.argsType match {
          case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
        }
        arg.outputView = fc.outputView.split(n)

        assert(arg.outputView != NoView)

        generateOutputView(arg)
      }

      case fc@FunCall(TransposeW(), arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView
        fc.t match {
          case ArrayTypeWS(ArrayTypeWS(typ, m), n) =>
            //working
            //arg.outputView = fc.outputView.join(m).reorder( (i: ArithExpr) => { transpose(i, ArrayTypeWSWC(ArrayTypeWSWC(typ, n), m)) }).split(n)
            //experimental
            arg.outputView = fc.outputView.transpose(fc.t)
          case _ => assert(false, "Other types other than 2D array are not allowed for TransposeW()")
        }

        assert(arg.outputView != NoView)

        generateOutputView(arg)

      }

      case fc@FunCall(_:UserFun, args@_*) => {

        assert(fc.outputView != NoView)

        args.foreach (
          arg => arg match {
            //case _:Value =>
            case p:Param =>
              p.outputView = fc.outputView
            case fc_get@FunCall(_:Get, arg) =>
              fc_get.outputView = fc.outputView
            case _ => assert(false, "Some Type not implemented")
          }
        )

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach(generateOutputView(_) )

        /*
        //You could write test code directly here to see the offset
        val output_view_offset = layoutS(ViewPrinter(fc.outputView).print())

        println("output done")

        val input_view_offsets = mutable.ArrayBuffer.empty[String]
        args.foreach(arg => input_view_offsets += layoutS(ViewPrinter(arg.view).print())   )

        println("input done")
        */


      }

      case fc@FunCall(m:AbstractMap, arg) => {

        assert(fc.outputView != NoView)

        //this line reflect the map semantic
        m.f.body.outputView = fc.outputView.access(m.loopVar)

        generateOutputView(m.f.body)

        arg.outputView = ViewMap(m.f.params.head.outputView, m.loopVar, arg.t)

        assert(arg.outputView != NoView)

        generateOutputView(arg)


      }

      case fc@FunCall(r: AbstractPartRed, args@_*) => {

        assert(fc.outputView != NoView)

        assert(args.length == 2)

        r.f.body.outputView = fc.outputView.access(Cst(0))

        generateOutputView(r.f.body)

        val acc = args(0)
        val array = args(1)

        acc.outputView = UnusedInExprOutputView
        array.outputView = ViewMap(r.f.params(1).outputView, r.loopVar, array.t)

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach(generateOutputView(_))


        /*val acc = args.head
        acc.outputView = ViewMem(acc.mem.variable, acc.t)
        args(1).outputView = r.f.params.head.outputView

        generateOutputView(args(1)) */

      }

      case fc@FunCall(_, arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        generateOutputView(arg)
      }

      case _ =>
    }
  }

  def apply(lambda: Lambda): Unit = {

    lambda visitBy {
        case e:Expr => assert(e.outputView == NoView)
        case _ =>
    }

    //first set the body's output view, then propagate to someone inside.
    lambda.body.outputView = ViewMem(lambda.body.mem.variable, lambda.body.t)

    generateOutputView(lambda.body)

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
        case e:Expr => assert( e.outputView != NoView )
        case _ =>
    }

  }

}
