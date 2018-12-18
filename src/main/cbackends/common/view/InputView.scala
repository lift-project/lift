package cbackends.common.view

import ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC, TupleType}
import ir.ast.{AbstractMap, AbstractPartRed, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Expr, FunCall, Get, IRNode, Join, Lambda, Pad, Param, Split, Transpose, TransposeW, UserFun, Value, Zip, transpose}
import ir.view._
import cbackends.common.utils.input_view.InputView.{init_params, post_check, pre_check}

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

  def generateInputView(node: Option[IRNode], cont: Option[IRNode] => Option[IRNode]): Option[IRNode] = {
    node match {

      case None => None


      case Some(a@ArrayFromUserFunGenerator(f, at) ) =>   a.view = ViewGeneratorUserFun(f, at); None
      case Some(a@Array2DFromUserFunGenerator(f, at) ) => a.view = View2DGeneratorUserFun(f, at); None
      case Some(a@Array3DFromUserFunGenerator(f, at) ) => a.view = View3DGeneratorUserFun(f, at); None

      case Some(v: Value) => v.view = ViewConstant(v, v.t); None
      //In the composable pattern matching, all match has to be explicit, even though they do nothing
      case Some(_: Param) => None


      case Some(fc@FunCall(_:Zip, args@_*)) => {

        args.foreach( a => cont( Some(a)  ) )

        val input_view = getViewFromArgs(fc)
        fc.view = input_view.zip()

        None

      }

      case Some(fc@FunCall(Get(n), arg) ) => {

        cont( Some(arg) )

        fc.view = arg.view.get(n)

        None

      }


      case Some(fc@FunCall(Split(n), arg) ) => {
        cont( Some(arg) )
        fc.view = arg.view.split(n)

        None
      }
      case Some(fc@FunCall(_:Join, arg) ) => {

        cont( Some(arg) )

        val n = fc.argsType match {
          case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
        }
        fc.view = arg.view.join(n)

        None
      }

      case Some(fc@FunCall(_:Transpose, arg) ) => {

        cont( Some(arg) )
        fc.t match{
          case ArrayTypeWS(ArrayTypeWS(typ, m), n) =>
            //working
            //fc.view = arg.view.join(n).reorder( (i: ArithExpr) => { transpose(i, fc.t) }  ).split(m)
            //experimental
            fc.view = arg.view.transpose(fc.t)
          case _ => assert(false, "Other types other than 2D array are not allowed for transpose")
        }

        None

      }

      case Some(fc@FunCall(Pad(left, right, boundaryFun), arg) ) => {

        cont( Some(arg) )
        fc.view = arg.view.pad(left, right, boundaryFun)

        None

      }

      case Some(fc@FunCall(_:UserFun, args@_*) ) => {

        args.foreach( a => cont( Some(a) ))

        fc.view = ViewMem(fc.mem.variable, fc.t)

        None

      }

      case Some(fc@FunCall(m:AbstractMap, arg) ) => {

        cont( Some(arg) )

        //this line reflect the map semantic
        m.f.params.head.view = arg.view.access(m.loopVar)

        cont( Some(m.f.body) )

        //Take the courage to use the simplest solution seen so far, keep in mind that this might break
        fc.view = ViewMap(m.f.body.view, m.loopVar, fc.t)

        None

      }

      case Some(fc@FunCall(r: AbstractPartRed, args@_*) )  => {

        args.foreach( a => cont( Some(a) ))

        val input_view = getViewFromArgs(fc)

        r.f.params(0).view = input_view.get(0)
        r.f.params(1).view = input_view.get(1).access(r.loopVar)

        cont( Some(r.f.body) )

        //No need to initialize a new view, as the view is the same as its inner view
        //In Map, the memory is augmented for its user function, thus in that case a new view is needed
        //but it is not the case for reduce.
        fc.view = r.f.body.view

        None

      }


      case Some(fc@FunCall(_:TransposeW, arg) ) => {

        cont( Some(arg) )

        fc.view = arg.view

        None

      }

      case Some(_) => node
    }
  }


  def default_generateInputView(in: Option[IRNode]) : Option[IRNode] = {
    val partial_binded = generateInputView(_:Option[IRNode], default_generateInputView)
    val composed = partial_binded andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    composed(in)
  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_params(lambda)

    default_generateInputView(Some(lambda.body) )

    post_check(lambda)


  }

}
