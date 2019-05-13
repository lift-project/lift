package cbackends.common.view

import cbackends.common.common_ir.CPUNullMemory
import cbackends.common.utils.common_view.GenerateViewForRawInOut
import ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC}
import ir.ast.{AbstractMap, AbstractPartRed, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayAccess, ArrayFromUserFunGenerator, Expr, FunCall, Get, IRNode, Iterate, Join, Lambda, Pad, Param, Slide, Split, Transpose, TransposeW, UserFun, Value, Zip, transpose}
import ir.view._
import lift.arithmetic.{ArithExpr, Cst}
import core.generator.PrettyPrinter._
import cbackends.common.utils.output_view.OutputView.{init_body, post_check, pre_check}
import cbackends.common.common_ir.HostMemory
import cbackends.host.host_ir._
import opencl.ir.pattern.ScanSeq


object OutputView {

  def generateOutputView(node: IRNode, cont: IRNode => IRNode) : IRNode = {
    node match {

      //In the composable pattern matching, all match has to be explicit, even though they do nothing
      case v:Value => v
      case p:Param => p

      case a@ArrayFromUserFunGenerator(f, at)  =>
        //a.outputView = ViewGeneratorUserFun(f, at); a
        a.outputView = GenerateViewForRawInOut.generateViewForRawInOut(a, a.t, Cst(1)); a
      case a@Array2DFromUserFunGenerator(f, at)  =>
        //a.outputView = View2DGeneratorUserFun(f, at); a
        a.outputView = GenerateViewForRawInOut.generateViewForRawInOut(a, a.t, Cst(1)); a
      case a@Array3DFromUserFunGenerator(f, at)  =>
        //a.outputView = View3DGeneratorUserFun(f, at); a
        a.outputView = GenerateViewForRawInOut.generateViewForRawInOut(a, a.t, Cst(1)); a

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

      case fc@FunCall(Get(_)|ArrayAccess(_), arg)  => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont(arg)

        fc
      }


      case fc@FunCall(Split(n), arg)  => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView.join(n)
        /*
        fc.isConcrete match {
          case true => arg.outputView = fc.outputView.join(n)
          case false => arg.outputView = UnusedInExprOutputView
        }*/

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

        /*
      case fc@FunCall(_:CPUFunCall, arg) => {

        arg.outputView = ViewMem(arg.mem.variable, arg.t)

        cont( arg )

        fc
      }*/

      case fc@FunCall(_:CPUFunContainer | _:OclFunContainer | _:ToGPU | _:ToHost, args@_*) => {

        args.foreach( arg => arg.outputView = ViewMem(arg.mem.variable, arg.t) )

        args.foreach( cont(_) )

        fc
      }

      case fc@FunCall(_:UserFun, args@_*)  => {

        assert(fc.outputView != NoView)

        args.foreach (
          arg => arg match {
            case v:Value =>
              v.outputView = UnusedInExprOutputView
            case p:Param =>
              //p.outputView = fc.outputView
              p.mem match {
                case CPUNullMemory =>
                  p.outputView = UnusedInExprOutputView
                case _:HostMemory =>
                  p.outputView = ViewMem(arg.mem.variable, arg.t)
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

      case fc@FunCall(i:Iterate, arg) => {

        i.f.body.outputView = fc.outputView

        cont(i.f.body)

        arg.outputView = i.f.params.head.outputView

        cont(arg)

        fc
      }

      case fc@FunCall(m:AbstractMap, arg) => {

        assert(fc.outputView != NoView)

        /*fc.isConcrete match {
            //stop updating output view if not concrete
          case false =>
            m.f.body.outputView = UnusedInExprOutputView
            //this line reflect the map semantic
          case true =>
            m.f.body.outputView = fc.outputView.access(m.loopVar)
        }*/
        m.f.body.outputView = fc.outputView.access(m.loopVar)

        cont(m.f.body)

        /*
        //arg.outputView = ViewMap(m.f.params.head.outputView, m.loopVar, arg.t)
        fc.isConcrete match {
          case false =>
            arg.outputView = UnusedInExprOutputView
          case true =>
            arg.outputView = m.f.params.head.outputView match {
            //case ViewMem(v, _) => ViewMem(v, arg.t)
            case ViewMem(v, _) =>
              GenerateViewForRawInOut.generateViewForRawInOut(arg, arg.t, Cst(1))
            case outputView =>
              val t = fc.argsType
              val chunksize  = t match {
                case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
                case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
              }
              //outputView.split(chunksize)
              ViewSplit(chunksize, outputView, t)
          }
        }*/
        arg.outputView = m.f.params.head.outputView match {
          //case ViewMem(v, _) => ViewMem(v, arg.t)
          case ViewMem(v, _) =>
            GenerateViewForRawInOut.generateViewForRawInOut2(arg, arg.t, Cst(1))
          case outputView =>
            val t = fc.argsType
            val chunksize  = t match {
              case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
              case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
            }
            //outputView.split(chunksize)
            ViewSplit(chunksize, outputView, t)
        }

        assert(arg.outputView != NoView)

        cont(arg)

        fc


      }

      case fc@FunCall(r: AbstractPartRed, args@_*) => {

        assert(fc.outputView != NoView)

        assert(args.length == 2)

        /*fc.isConcrete match {
          //stop updating output view if not concrete
          case false =>
            r.f.body.outputView = UnusedInExprOutputView
          case true => r.f.body.outputView = fc.outputView.access(Cst(0))
        }*/
        r.f.body.outputView = fc.outputView.access(Cst(0))

        cont( r.f.body )

        val acc = args(0)
        val array = args(1)

        acc.outputView = UnusedInExprOutputView

        /*
        fc.isConcrete match {
          case false =>
            r.f.params.head.outputView = UnusedInExprOutputView
          case true =>
            //may need a case hanlder in the future, if the inner part is already an array, you may need to generate a split
            //currently it is only a float, so just use the array's t is OK.
            //array.outputView = ViewMap(r.f.params(1).outputView, r.loopVar, array.t)
            array.outputView = r.f.params(1).outputView match {
              //case ViewMem(v, _) => ViewMem(v, array.t)
              case ViewMem(v, _) =>
                GenerateViewForRawInOut.generateViewForRawInOut(array, array.t, Cst(1))
              case outputView =>
                val t = fc.argsType
                val chunksize = t match {
                  case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
                  case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
                }
                outputView.split(chunksize)
            }
        } */
        array.outputView = r.f.params(1).outputView match {
          //case ViewMem(v, _) => ViewMem(v, array.t)
          case ViewMem(v, _) =>
            GenerateViewForRawInOut.generateViewForRawInOut(array, array.t, Cst(1))
          case outputView =>
            val t = fc.argsType
            val chunksize = t match {
              case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
              case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
            }
            outputView.split(chunksize)
        }

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach( cont(_) )


        /*val acc = args.head
        acc.outputView = ViewMem(acc.mem.variable, acc.t)
        args(1).outputView = r.f.params.head.outputView

        generateOutputView(args(1)) */

        fc

      }


      case fc@FunCall(s:ScanSeq, args@_*) => {

        assert(fc.outputView != NoView)

        assert(args.length == 2)

        s.f.body.outputView = fc.outputView.access(s.loopVar)

        cont(s.f.body)

        val acc = args(0)
        val array = args(1)

        acc.outputView = UnusedInExprOutputView

        array.outputView = s.f.params(1).outputView match {
          //case ViewMem(v, _) => ViewMem(v, arg.t)
          case ViewMem(v, _) =>
            GenerateViewForRawInOut.generateViewForRawInOut(array, array.t, Cst(1))
          case outputView =>
            val t = fc.argsType
            val chunksize  = t match {
              case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
              case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
            }
            //outputView.split(chunksize)
            ViewSplit(chunksize, outputView, t)
        }

        args.foreach(a => assert(a.outputView != NoView))

        args.foreach( cont(_) )

        fc


      }

      case fc@FunCall(_:Slide, arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView match {
          case vs:ViewSplit =>
            vs.iv
          case _ =>
            fc.outputView
        }

        assert(arg.outputView != NoView)

        cont( arg )

        fc
      }


      case fc@FunCall(_:Transpose, arg) => {

        assert(fc.outputView != NoView)


        /*
        arg.t match{
          case ArrayTypeWS(ArrayTypeWS(typ, m), n) =>
            //working
            arg.outputView= fc.outputView.join(n).reorder( (i: ArithExpr) => { transpose(i, arg.t) }  ).split(m)
          //experimental
          //fc.view = arg.view.transpose(fc.t)
          case _ => assert(false, "Other types other than 2D array are not allowed for transpose")
        }*/

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont( arg )

        fc
      }


      case fc@FunCall(_:Pad, arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView
        //arg.outputView = UnusedInExprOutputView

        assert(arg.outputView != NoView)

        cont( arg )

        fc
      }
/*
      case fc@FunCall(_:Slide, arg) => {

        arg.outputView = fc.outputView

        cont( arg )

        fc
      }*/



      case fc@FunCall(l:Lambda, args@_*) => {


        fc.isConcrete match {
          //stop updating output view if not concrete
          case false =>
            l.body.outputView = UnusedInExprOutputView
          //this line reflect the map semantic
          case true =>  l.body.outputView = fc.outputView
        }

        cont(l.body)

        //arg.outputView = ViewMap(m.f.params.head.outputView, m.loopVar, arg.t)
        fc.isConcrete match {
          case false =>
            args.foreach( _.outputView = UnusedInExprOutputView )
          case true => (args zip l.params).foreach { case Tuple2(arg, param) =>
            arg.outputView = param.outputView match {
              //case ViewMem(v, _) => ViewMem(v, arg.t)
              case ViewMem(v, _) => GenerateViewForRawInOut.generateViewForRawInOut(arg, arg.t, Cst(1))
              case x => x
            }
          }
        }


        args.foreach( cont(_) )

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
