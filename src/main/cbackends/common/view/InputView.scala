package cbackends.common.view

import ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC, TupleType}
import ir.ast.{AbstractMap, AbstractPartRed, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Expr, FunCall, Get, IRNode, Join, Lambda, Pad, Param, Split, Transpose, TransposeW, UserFun, Value, Zip, transpose}
import ir.view._
import cbackends.common.utils.input_view.InputView.{init_params, post_check, pre_check}
import cbackends.common.utils.pattern_matching.IsDefinedAt

object InputView {

  private def getViewFromArgs(fc: FunCall): View = {
    if (fc.args.isEmpty) {
      assert(false)
      NoView
    } else if (fc.args.length == 1) {
      fc.args.head.view
    } else {
      val input_views = fc.args.map(_.view)
      ViewTuple(input_views, TupleType(input_views.map(_.t): _*))
    }
  }

  def generateInputView(node: IRNode, cont: IRNode => IRNode): IRNode = {
    node match {



      case a@ArrayFromUserFunGenerator(f, at)  =>   a.view = ViewGeneratorUserFun(f, at); a
      case a@Array2DFromUserFunGenerator(f, at)  => a.view = View2DGeneratorUserFun(f, at); a
      case a@Array3DFromUserFunGenerator(f, at)  => a.view = View3DGeneratorUserFun(f, at); a

      case v: Value => v.view = ViewConstant(v, v.t); v
      //In the composable pattern matching, all match has to be explicit, even though they do nothing
      case p: Param => p


      case fc@FunCall(_:Zip, args@_*) => {

        args.foreach( a => cont( a  ) )

        val input_view = getViewFromArgs(fc)
        fc.view = input_view.zip()

        fc

      }

      case fc@FunCall(Get(n), arg)  => {

        cont( arg )

        fc.view = arg.view.get(n)

        fc

      }


      case fc@FunCall(Split(n), arg)  => {
        cont( arg )
        fc.view = arg.view.split(n)

        fc
      }
      case fc@FunCall(_:Join, arg)  => {

        cont( arg )

        val n = fc.argsType match {
          case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
        }
        fc.view = arg.view.join(n)

        fc
      }

      case fc@FunCall(_:Transpose, arg)  => {

        cont( arg )
        fc.t match{
          case ArrayTypeWS(ArrayTypeWS(typ, m), n) =>
            //working
            //fc.view = arg.view.join(n).reorder( (i: ArithExpr) => { transpose(i, fc.t) }  ).split(m)
            //experimental
            fc.view = arg.view.transpose(fc.t)
          case _ => assert(false, "Other types other than 2D array are not allowed for transpose")
        }

        fc

      }

      case fc@FunCall(Pad(left, right, boundaryFun), arg)  => {

        cont( arg )
        fc.view = arg.view.pad(left, right, boundaryFun)

        fc

      }

      case fc@FunCall(_:UserFun, args@_*)  => {

        args.foreach( a => cont( a ))

        fc.view = ViewMem(fc.mem.variable, fc.t)

        fc

      }

      case fc@FunCall(m:AbstractMap, arg)  => {

        cont( arg )

        //this line reflect the map semantic
        m.f.params.head.view = arg.view.access(m.loopVar)

        cont( m.f.body)

        //Take the courage to use the simplest solution seen so far, keep in mind that this might break
        fc.view = ViewMap(m.f.body.view, m.loopVar, fc.t)

        fc

      }

      case fc@FunCall(r: AbstractPartRed, args@_*)   => {

        args.foreach( a => cont( a ))

        val input_view = getViewFromArgs(fc)

        r.f.params(0).view = input_view.get(0)
        r.f.params(1).view = input_view.get(1).access(r.loopVar)

        cont( r.f.body )

        //No need to initialize a new view, as the view is the same as its inner view
        //In Map, the memory is augmented for its user function, thus in that case a new view is needed
        //but it is not the case for reduce.
        fc.view = r.f.body.view

        fc

      }


      case fc@FunCall(_:TransposeW, arg)  => {

        cont( arg )

        fc.view = arg.view

        fc

      }

    }
  }


  def default_generateInputView(in: IRNode) : IRNode = {
    //val partial_binded = generateInputView(_:Option[IRNode], default_generateInputView)
    //val composed = partial_binded andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    //composed(in)

    //val partial_binded = new PartialFunction[IRNode, IRNode] with IsDefinedAt[IRNode] { def apply(x: IRNode) = generateInputView(x, default_generateInputView) }
    //partial_binded(in)

    generateInputView(in, default_generateInputView)

  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_params(lambda)

    default_generateInputView( lambda.body )

    post_check(lambda)


  }

}
