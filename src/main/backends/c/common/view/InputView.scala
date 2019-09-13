package backends.c.common.view

import backends.c.common.common_ir.Slice
import backends.c.common.utils.common_view.GenerateViewForRawInOut
import ir._
import ir.ast.{AbstractMap, AbstractPartRed, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayAccess, ArrayFromUserFunGenerator, Expr, FunCall, Get, IRNode, Iterate, Join, Lambda, Pad, Param, Slide, Split, Transpose, TransposeW, UserFun, Value, Zip, transpose}
import ir.view._
import backends.c.common.utils.input_view.InputView.{init_params, post_check, pre_check}
import backends.c.common.utils.pattern_matching.IsDefinedAt
import backends.c.host.host_ir._
import lift.arithmetic.{ArithExpr, Cst}
import opencl.ir.pattern.{MapSeq, ScanSeq}

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

        args.foreach( cont(_) )

        val input_view = getViewFromArgs(fc)
        fc.view = input_view.zip()

        fc

      }

      case fc@FunCall(Get(n), arg)  => {

        cont( arg )

        fc.view = arg.view.get(n)

        fc

      }

      case fc@FunCall(ArrayAccess(n), arg)  => {

        cont( arg )

        fc.view = arg.view.access(n)

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
            fc.view = arg.view.join(n).reorder( (i: ArithExpr) => { transpose(i, fc.t) }  ).split(m)
            //experimental
            //fc.view = arg.view.transpose(fc.t)
          case _ => assert(false, "Other types other than 2D array are not allowed for transpose")
        }

        fc

      }

      case fc@FunCall(Pad(left, right, boundaryFun), arg)  => {

        cont( arg )
        fc.view = arg.view.pad(left, right, boundaryFun)

        fc

      }

      case fc@FunCall(_:UserFun | _:OpaqueCPUFunc | _:OpaqueOclFunc | _:ToGPU | _:ToHost, args@_*)  => {

        args.foreach( cont(_) )

        fc.view = ViewMem(fc.mem.variable, fc.t)

        fc

      }

      case fc@FunCall(i:Iterate, arg) => {

        cont(arg)

        i.f.params.head.view = arg.view

        cont( i.f.body )

        fc.view = i.f.body.view

        fc

      }

      case fc@FunCall(m:ir.ast.Map, arg)  => {

        cont( arg )

        //this line reflect the map semantic
        m.f.params.head.view = arg.view.access(m.loopVar)

        cont( m.f.body)

        //fc.view = m.f.body.view
        fc.view = ViewMap(m.f.body.view, m.loopVar, fc.t)

        fc

      }

      case fc@FunCall(m:AbstractMap, arg)  => {

        cont( arg )

        //this line reflect the map semantic
        m.f.params.head.view = arg.view.access(m.loopVar)

        cont( m.f.body )

        fc.isConcrete match {
          case true => m.f.body.view match {
            case ViewMem(v, t) =>
              t match {
                case _:ScalarType | _:TupleType => fc.view = ViewMem(v, fc.t)
                case _:ArrayType => fc.view = GenerateViewForRawInOut.generateViewForRawInOut(fc, fc.t, Cst(1))
                case _ =>
              }
            case _ => fc.view = ViewMap(m.f.body.view, m.loopVar, fc.t)
          }
          case _ => fc.view = ViewMap(m.f.body.view, m.loopVar, fc.t)
        }

        fc

      }


        /*
      case fc@FunCall(m:AbstractMap, arg)  => {

        cont( arg )

        //this line reflect the map semantic
        m.f.params.head.view = arg.view.access(m.loopVar)

        cont( m.f.body)

        //Take the courage to use the simplest solution seen so far, keep in mind that this might break
        fc.view = ViewMap(m.f.body.view, m.loopVar, fc.t)

        fc

      }*/

      case fc@FunCall(r: AbstractPartRed, args@_*)   => {

        args.foreach( cont(_) )

        val input_view = getViewFromArgs(fc)

        r.f.params(0).view = input_view.get(0)
        r.f.params(1).view = input_view.get(1).access(r.loopVar)

        cont( r.f.body )

        fc.view = r.f.body.view

        fc.isConcrete match {
          case true => r.f.body.view match {
            case ViewMem(v, t) =>
              t match {
                  //if it is a scalar, fc.t will be []_1, just works as in map
                case _:ScalarType | _:TupleType => fc.view = ViewMem(v, fc.t)
                case _:ArrayType => fc.view = GenerateViewForRawInOut.generateViewForRawInOut(fc, fc.t, Cst(1))
                case _ =>
              }
            case _ =>
          }
          case false =>
        }

        fc

      }

      case fc@FunCall(s: ScanSeq, args@_*)   => {

        //scan.f.params(0).view = argView.get(0)
        //scan.f.params(1).view = argView.get(1).access(scan.loopVar)

        //visitAndBuildViews(scan.f.body)

        //View.initialiseNewView(call.t, call.inputDepth, call.mem.variable)

        args.foreach( cont(_) )

        val input_view = getViewFromArgs(fc)

        s.f.params(0).view = input_view.get(0)
        s.f.params(1).view = input_view.get(1).access(s.loopVar)

        cont( s.f.body )

        fc.view = s.f.body.view

        fc.isConcrete match {
          case true => s.f.body.view match {
            case ViewMem(v, t) =>
              t match {
                //if it is a scalar, fc.t will be []_1, then you only need to allocate a new ViewMem of size 1
                case _:ScalarType | _:TupleType => fc.view = ViewMem(v, fc.t)
                case _:ArrayType => fc.view = GenerateViewForRawInOut.generateViewForRawInOut(fc, fc.t, Cst(1))
                case _ =>
              }
            case _ =>
          }
          case false =>
        }

        fc

      }



      case fc@FunCall(_:TransposeW, arg)  => {

        cont( arg )

        fc.view = arg.view

        fc

      }

      case fc@FunCall(s:Slide, arg) => {
        cont( arg )

        fc.view = arg.view.slide(s)

        fc
      }

      case fc@FunCall(s:Slice, arg) => {
        cont( arg )

        fc.view = arg.view.slice(s)

        fc
      }


      case fc@FunCall(l:Lambda, args@_*) => {

        args.foreach( cont(_) )

        (l.params zip args).foreach(pair => pair._1.view = pair._2.view)

        cont( l.body )

        fc.view = l.body.view

        fc

      }

      /*case x:Expr =>
        assert(false)
        x */

    }
  }


  def default_generateInputView(in: IRNode) : IRNode = {

    generateInputView(in, default_generateInputView)

  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_params(lambda)

    default_generateInputView( lambda.body )

    post_check(lambda)


  }

}
