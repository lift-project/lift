package cbackends.common.view

import cbackends.common.common_ir.CPUNullMemory
import cbackends.common.utils.common_view.GenerateViewForRawInOut
import ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC}
import ir.ast.{AbstractMap, AbstractPartRed, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Expr, FunCall, Get, IRNode, Join, Lambda, Pad, Param, Split, Transpose, TransposeW, UserFun, Value, Zip, transpose}
import ir.view._
import lift.arithmetic.{ArithExpr, Cst}
import core.generator.PrettyPrinter._
import cbackends.common.utils.output_view.OutputView.{init_body, post_check, pre_check}
import host.ir_host.HostMemory


object OutputView {

  def generateOutputView(node: IRNode, cont: IRNode => IRNode) : IRNode = {
    node match {

      //In the composable pattern matching, all match has to be explicit, even though they do nothing
      case v:Value => v
      case p:Param => p

      case a@ArrayFromUserFunGenerator(f, at)  =>
        a.outputView = ViewGeneratorUserFun(f, at); a
      case a@Array2DFromUserFunGenerator(f, at)  =>
        a.outputView = View2DGeneratorUserFun(f, at); a
      case a@Array3DFromUserFunGenerator(f, at)  =>
        a.outputView = View3DGeneratorUserFun(f, at); a

      case fc@FunCall(_:Zip, args@_*)  => {

        assert(fc.outputView != NoView)

        val result = fc.outputView.unzip()

        args.zipWithIndex.foreach({
          //case (arg: Param, id) if arg.outputView == NoView =>
          case (arg: Expr, id) if arg.outputView == NoView =>
            arg.outputView = result.get(id)
          case _ =>
        })

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach(cont(_))

        fc

      }

      case fc@FunCall(Get(n), arg)  => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont(arg)

        fc
      }


      case fc@FunCall(Split(n), arg)  => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView.join(n);

        assert(arg.outputView != NoView)

        cont(arg)

        fc
      }
      case fc@FunCall(_:Join, arg)  => {

        assert(fc.outputView != NoView)


        val n = fc.argsType match {
          case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
        }
        arg.outputView = fc.outputView.split(n)

        assert(arg.outputView != NoView)

        cont( arg )

        fc
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

        cont( arg )

        fc

      }

      case fc@FunCall(_:UserFun, args@_*)  => {

        assert(fc.outputView != NoView)

        args.foreach (
          arg => arg match {
            //case _:Value =>
            case p:Param =>
              //p.outputView = fc.outputView
              p.mem match {
                case _:HostMemory => p.outputView = ViewMem(arg.mem.variable, arg.t)
                case CPUNullMemory =>
                case _ => assert(false)
              }
            case fc_get@FunCall(_:Get, arg) =>
              //fc_get.outputView = fc.outputView
              fc_get.outputView = ViewMem(fc.mem.variable, fc.t)
            case _ => assert(false, "Some Type not implemented")
          }
        )

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach( cont(_) )

        /*
        //You could write test code directly here to see the offset
        val output_view_offset = layoutS(ViewPrinter(fc.outputView).print())

        println("output done")

        val input_view_offsets = mutable.ArrayBuffer.empty[String]
        args.foreach(arg => input_view_offsets += layoutS(ViewPrinter(arg.view).print())   )

        println("input done")
        */

        fc

      }

      case fc@FunCall(m:AbstractMap, arg) => {

        assert(fc.outputView != NoView)

        //this line reflect the map semantic
        m.f.body.outputView = fc.outputView.access(m.loopVar)

        cont(m.f.body)

        //arg.outputView = ViewMap(m.f.params.head.outputView, m.loopVar, arg.t)
        arg.outputView = m.f.params.head.outputView match {
          //case ViewMem(v, _) => ViewMem(v, arg.t)
          case ViewMem(v, _) => GenerateViewForRawInOut.generateViewForRawInOut(arg, arg.t, Cst(1))
          case x => x
        }

        assert(arg.outputView != NoView)

        cont(arg)

        fc


      }

      case fc@FunCall(r: AbstractPartRed, args@_*) => {

        assert(fc.outputView != NoView)

        assert(args.length == 2)

        r.f.body.outputView = fc.outputView.access(Cst(0))

        cont( r.f.body )

        val acc = args(0)
        val array = args(1)

        acc.outputView = UnusedInExprOutputView
        //may need a case hanlder in the future, if the inner part is already an array, you may need to generate a split
        //currently it is only a float, so just use the array's t is OK.
        //array.outputView = ViewMap(r.f.params(1).outputView, r.loopVar, array.t)
        array.outputView = r.f.params(1).outputView  match {
          //case ViewMem(v, _) => ViewMem(v, array.t)
          case ViewMem(v, _) => GenerateViewForRawInOut.generateViewForRawInOut(array, array.t, Cst(1))
          case x => x
        }

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach( cont(_) )


        /*val acc = args.head
        acc.outputView = ViewMem(acc.mem.variable, acc.t)
        args(1).outputView = r.f.params.head.outputView

        generateOutputView(args(1)) */

        fc

      }



      case fc@FunCall(_:Transpose|_:Pad, arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont( arg )

        fc
      }

    }
  }

  def default_generateOutputView(in: IRNode) : IRNode = {
    generateOutputView(in, default_generateOutputView)
  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_body(lambda)

    default_generateOutputView( lambda.body )

    post_check(lambda)


  }

}
