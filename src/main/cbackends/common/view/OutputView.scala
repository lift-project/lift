package cbackends.common.view

import ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC}
import ir.ast.{AbstractMap, AbstractPartRed, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Expr, FunCall, Get, IRNode, Join, Lambda, Param, Split, TransposeW, UserFun, Value, Zip, transpose}
import ir.view._
import lift.arithmetic.{ArithExpr, Cst}
import core.generator.PrettyPrinter._
import cbackends.common.utils.output_view.OutputView.{pre_check,post_check,init_body}


object OutputView {

  def generateOutputView(node: Option[IRNode], cont: Option[IRNode] => Option[IRNode]) : Option[IRNode] = {
    node match {

      case None => None

      //In the composable pattern matching, all match has to be explicit, even though they do nothing
      case Some(_:Value) => None
      case Some(_:Param) => None

      case Some(a@ArrayFromUserFunGenerator(f, at) ) =>
        a.outputView = ViewGeneratorUserFun(f, at); None
      case Some(a@Array2DFromUserFunGenerator(f, at))  =>
        a.outputView = View2DGeneratorUserFun(f, at); None
      case Some(a@Array3DFromUserFunGenerator(f, at) ) =>
        a.outputView = View3DGeneratorUserFun(f, at); None

      case Some(fc@FunCall(_:Zip, args@_*) ) => {

        assert(fc.outputView != NoView)

        val result = fc.outputView.unzip()

        args.zipWithIndex.foreach({
          //case (arg: Param, id) if arg.outputView == NoView =>
          case (arg: Expr, id) if arg.outputView == NoView =>
            arg.outputView = result.get(id)
          case _ =>
        })

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach(a => cont( Some(a)))

        None

      }

      case Some(fc@FunCall(Get(n), arg) ) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont(Some(arg))

        None
      }


      case Some(fc@FunCall(Split(n), arg) ) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView.join(n);

        assert(arg.outputView != NoView)

        cont(Some(arg))

        None
      }
      case Some(fc@FunCall(_:Join, arg) ) => {

        assert(fc.outputView != NoView)


        val n = fc.argsType match {
          case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
        }
        arg.outputView = fc.outputView.split(n)

        assert(arg.outputView != NoView)

        cont( Some(arg) )

        None
      }

      case Some(fc@FunCall(TransposeW(), arg) ) => {

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

        cont( Some(arg) )

        None

      }

      case Some(fc@FunCall(_:UserFun, args@_*) ) => {

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

        args.foreach(a => cont(Some(a) ) )

        /*
        //You could write test code directly here to see the offset
        val output_view_offset = layoutS(ViewPrinter(fc.outputView).print())

        println("output done")

        val input_view_offsets = mutable.ArrayBuffer.empty[String]
        args.foreach(arg => input_view_offsets += layoutS(ViewPrinter(arg.view).print())   )

        println("input done")
        */

        None

      }

      case Some(fc@FunCall(m:AbstractMap, arg) ) => {

        assert(fc.outputView != NoView)

        //this line reflect the map semantic
        m.f.body.outputView = fc.outputView.access(m.loopVar)

        cont(Some(m.f.body) )

        arg.outputView = ViewMap(m.f.params.head.outputView, m.loopVar, arg.t)

        assert(arg.outputView != NoView)

        cont(Some(arg) )

        None


      }

      case Some(fc@FunCall(r: AbstractPartRed, args@_*) ) => {

        assert(fc.outputView != NoView)

        assert(args.length == 2)

        r.f.body.outputView = fc.outputView.access(Cst(0))

        cont( Some(r.f.body) )

        val acc = args(0)
        val array = args(1)

        acc.outputView = UnusedInExprOutputView
        array.outputView = ViewMap(r.f.params(1).outputView, r.loopVar, array.t)

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach(a => cont( Some(a) ))


        /*val acc = args.head
        acc.outputView = ViewMem(acc.mem.variable, acc.t)
        args(1).outputView = r.f.params.head.outputView

        generateOutputView(args(1)) */

        None

      }


      case Some(fc@FunCall(_, arg)) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont( Some(arg) )

        None
      }

      case Some(_) => node
    }
  }

  def default_generateOutputView(in: Option[IRNode]) : Option[IRNode] = {
    val partial_binded = generateOutputView(_:Option[IRNode], default_generateOutputView)
    val composed = partial_binded andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    composed(in)
  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_body(lambda)

    default_generateOutputView( Some(lambda.body) )

    post_check(lambda)


  }

}
