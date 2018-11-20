package host.view


import ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC, TupleType}
import ir.ast.{AbstractMap, AbstractPartRed, Expr, FunCall, Get, IRNode, Join, Lambda, Split, Transpose, UserFun, Value, Zip, transpose}
import ir.view._
import lift.arithmetic.ArithExpr

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

  def generateInputView(node: IRNode): Unit = {
    node match {


      case fc@FunCall(_:Zip, args@_*) => {

        args.foreach(generateInputView(_) )

        //val input_views = args.map(_.view)
        //fc.view = ViewTuple(input_views, TupleType(input_views.map(_.t): _*)).zip()

        val input_view = getViewFromArgs(fc)
        fc.view = input_view.zip()

      }

      case fc@FunCall(Get(n), arg) => {

        generateInputView(arg)

        fc.view = arg.view.get(n)

      }


      case fc@FunCall(Split(n), arg) => {
        generateInputView(arg);
        fc.view = arg.view.split(n) }
      case fc@FunCall(_:Join, arg) => {

        generateInputView(arg)

        val n = fc.argsType match {
          case ArrayType(ArrayTypeWSWC(_, s,c)) if s==c => s
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + fc.argsType)
        }
        fc.view = arg.view.join(n)
      }

      case fc@FunCall(_:Transpose, arg) => {

        generateInputView(arg)
        fc.t match{
          case ArrayTypeWS(ArrayTypeWS(typ, m), n) => fc.view = arg.view.join(n).reorder( (i: ArithExpr) => { transpose(i, fc.t) }  ).split(m)
          case _ => assert(false, "Other types other than 2D array are not allowed for transpose")
        }

      }

      case fc@FunCall(_:UserFun, args@_*) => {

        args.foreach(generateInputView(_))

        fc.view = ViewMem(fc.mem.variable, fc.t)

      }

      case fc@FunCall(m:AbstractMap, arg) => {

        generateInputView(arg)

        //this line reflect the map semantic
        m.f.params.head.view = arg.view.access(m.loopVar)

        generateInputView(m.f.body)

        //Take the courage to use the simplest solution seen so far, keep in mind that this might break
        fc.view = ViewMap(m.f.body.view, m.loopVar, fc.t)

      }

      case fc@FunCall(r: AbstractPartRed, args@_*) => {

        val input_view = getViewFromArgs(fc)

        r.f.params(0).view = input_view.get(0)
        r.f.params(1).view = input_view.get(1).access(r.loopVar)

        generateInputView(r.f.body)

        //TODO: not understand yet, according to the test run it seems we need to create a new view here
        fc.view = ViewMem(fc.mem.variable, fc.t)

      }

      case fc@FunCall(_, arg) => {

        generateInputView(arg)

        fc.view = arg.view

      }

      case _ =>
    }
  }

  def apply(lambda: Lambda): Unit = {

    lambda.visit(pre = {node:IRNode =>
      node match {
        case e:Expr => assert(e.view == NoView)
        case _ =>

      }
    })

    lambda.params.foreach( p => p.view = ViewMem(p.mem.variable, p.t) )

    generateInputView(lambda.body)

    lambda.visit(pre = {node:IRNode =>
      node match {
        case e:Expr if !e.isInstanceOf[Value] => assert( e.view != NoView )
        case _ =>
      }
    })

  }

}
