package host.view


import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast.{AbstractMap, AbstractPartRed, Expr, FunCall, Get, IRNode, Join, Lambda, Param, Split, UserFun, Value, Zip}
import ir.view.{NoView, ViewMap, ViewMem}
import lift.arithmetic.Cst
import core.generator.PrettyPrinter._

import scala.collection.mutable

object OutputView {

  def generateOutputView(node: IRNode): Unit = {
    node match {



      case fc@FunCall(_:Zip, args@_*) => {

        val result = fc.outputView.unzip()

        args.zipWithIndex.foreach({
          case (arg: Param, id) if arg.outputView == NoView => arg.outputView = result.get(id)
          case _ =>
        })

        args.foreach(generateOutputView(_))


      }

      case fc@FunCall(Get(n), arg) => {
        arg.outputView = fc.outputView
        generateOutputView(arg)
      }


      case fc@FunCall(Split(n), arg) => {
        arg.outputView = fc.outputView.join(n);
        generateOutputView(arg)
      }
      case fc@FunCall(_:Join, arg) => {

        val n = fc.argsType match {
          case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
        }
        arg.outputView = fc.outputView.split(n)

        generateOutputView(arg)
      }

      case fc@FunCall(_:UserFun, args@_*) => {

        args.foreach (
          arg => arg match {
            case p:Param =>
              p.outputView = fc.outputView
            case _:Value =>
            case fc_get@FunCall(_:Get, arg) =>
              fc_get.outputView = fc.outputView
            case _ => assert(false, "Some Type not implemented")
          }
        )

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



        //this line reflect the map semantic
        m.f.body.outputView = fc.outputView.access(m.loopVar)

        generateOutputView(m.f.body)

        arg.outputView = ViewMap(m.f.params.head.outputView, m.loopVar, arg.t)

        generateOutputView(arg)


      }

      case fc@FunCall(r: AbstractPartRed, args@_*) => {

        r.f.body.outputView = fc.outputView.access(Cst(0))

        generateOutputView(r.f.body)

        assert(args.length == 2)
        val acc = args.head
        acc.outputView = ViewMem(acc.mem.variable, acc.t)
        args(1).outputView = r.f.params.head.outputView

        generateOutputView(args(1))

      }

      case _ =>
    }
  }

  def apply(lambda: Lambda): Unit = {

    lambda.visit(pre = {node:IRNode =>
      node match {
        case e:Expr => assert(e.outputView == NoView)
        case _ =>

      }
    })

    //first set the body's output view, then propagate to someone inside.
    lambda.body.outputView = ViewMem(lambda.body.mem.variable, lambda.body.t)

    generateOutputView(lambda.body)

    lambda.visit(pre = {node:IRNode =>
      node match {
        case e:Expr => assert( e.outputView != NoView )
        case _ =>
      }
    })

  }

}
