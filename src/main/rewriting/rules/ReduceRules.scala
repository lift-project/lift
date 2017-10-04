package rewriting.rules

import ir.ast._
import lift.arithmetic.{?, ArithExpr}
import rewriting.utils.Utils

object ReduceRules {

  val partialReduce = Rule("Reduce(f) => Reduce(f) o PartRed(f)", {
    case FunCall(Reduce(f), init: Value, arg) =>

      // Need to replace the parameters for the partial reduce
      // Otherwise the 2 will end up sharing and give the wrong result
      val newAcc = Param()
      val newElem = Param()

      val expr = Expr.replace(f.body, f.params.head, newAcc)
      val finalExpr = Expr.replace(expr, f.params(1), newElem)

      // Make sure both get different objects, otherwise
      // there can be problems with sharing
      val initCopy = init.copy

      Reduce(f, init) o
        PartRed(Lambda(Array(newAcc, newElem), finalExpr), initCopy) $ arg
  })

  val partialReduceToReduce = Rule("PartRed(f) => Reduce(f)", {
    case FunCall(PartRed(f), init, arg) =>
      Reduce(f, init) $ arg
  })

  val partialReduceReorder: Rule = partialReduceReorder(?)

  def partialReduceReorder(s: ArithExpr): Rule =
    Rule("PartRed(f) => PartRed(f) o Reorder", {
      case FunCall(PartRed(f), init, arg) =>
        val stride = Utils.splitVariable(s, arg.t)
        PartRed(f, init) o Gather(ReorderWithStride(stride)) $ arg
    })

  // TODO: iterate
//  val partialReduceIterate: Rule = partialReduceIterate(?)
//
//  def partialReduceIterate(s: ArithExpr): Rule =
//    Rule("PartRed(f) => iterate(s)(PartRed(f))", {
//      case FunCall(PartRed(f), init, arg) =>
//        val iter = if (s == ?) Cst(1) else s
//        Iterate(iter)(PartRed(f, init)) $ arg
//    })


  val partialReduceSplitJoin: Rule = partialReduceSplitJoin(?)

  def partialReduceSplitJoin(split: ArithExpr): Rule =
    Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
      case FunCall(PartRed(f), init, arg) =>
        val chunkSize = Utils.splitVariable(split, arg.t)
        Join() o Map(PartRed(f, init)) o Split(chunkSize) $ arg
    })
}
