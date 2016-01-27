package rewriting.utils

import ir.ast._
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}

object LatexPrinter {

  def apply(expr: Expr): String = {
    expr match {
      case funCall: FunCall =>

        val calls = getCallsAtLevel(funCall)

        val ss =
          if (calls.length > 1)
            "(" + calls.map(c => {
              var string = apply(c.f)

              if (c.f.isInstanceOf[AbstractPartRed])
                string += " " + apply(c.args.head)

              string
            }).mkString(" \\circ ") + ")"
          else
            apply(calls.head.f)

        ss + " (" + calls.last.args.map(apply).mkString(", ") +  ")"



      case _ => expr.toString
    }
  }

  private def getCallsAtLevel(expr: Expr): Seq[FunCall] = {
    expr match {
      case call: FunCall =>
        if (call.args.length != 1
          && !call.f.isInstanceOf[UserFun]){

          if (call.f.isInstanceOf[AbstractPartRed])
            call +: getCallsAtLevel(call.args(1))
          else
            Seq(call)

        }
        else
          Seq(call)
      case _ => Seq()
    }
  }

  def apply(funDecl: FunDecl): String = {
    funDecl match {
      case lambda: Lambda => s"\\lambda ${
        if (lambda.params.length > 1)
          "(" + lambda.params.map(apply).mkString(", ") + ")"
        else
          apply(lambda.params.head)
      } . ${apply(lambda.body)}"
      case map: Map => s"Map(${apply(map.f)})"
      case mapSeq: MapSeq => s"MapSeq(${apply(mapSeq.f)})"
      case reduce: Reduce => s"Reduce(${apply(reduce.f)})"
      case reduceSeq: ReduceSeq => s"ReduceSeq(${apply(reduceSeq.f)})"
      case partRed: PartRed => s"PartialReduce(${apply(partRed.f)}})"
      case toGlobal: toGlobal => s"toGlobal(${apply(toGlobal.f)})"
      case uf: UserFun =>
        if (uf eq add)
          "+"
        else if (uf eq mult)
          "*"
        else
          uf.toString
      case _ => funDecl.toString
    }
  }

}
