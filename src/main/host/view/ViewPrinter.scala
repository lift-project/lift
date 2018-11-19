package host.view


import core.generator.GenericAST.{ArithExpression, ExpressionT, IntConstant, VarRef}
import ir.view.{View, ViewAccess, ViewJoin, ViewMap, ViewMem, ViewSplit, ViewTuple, ViewTupleComponent, ViewZip}
import lift.arithmetic.ArithExpr

object ViewPrinter {


  def generateArrayAccess(view: View,
                          arrayAccessStack: List[ArithExpr],
                          tupleAccessStack: List[Int]): ExpressionT = {
    view match {

      case ViewAccess(i, iv, _) =>
        generateArrayAccess(iv, i::arrayAccessStack, tupleAccessStack)

      case ViewSplit(chunksize, iv, _) =>
        val chunkIdx :: elemIdx :: rest = arrayAccessStack
        val newIdx = chunkIdx * chunksize + elemIdx
        generateArrayAccess(iv, newIdx::rest, tupleAccessStack)

      case ViewMem(memVar, ty) =>
        tupleAccessStack.isEmpty match {
          case true => assert(arrayAccessStack.size == 1);VarRef(memVar, arrayIndex = Some( ArithExpression(arrayAccessStack.head) ))
          case false => assert(false, "Not implement"); IntConstant(0)
        }
      /*
      assert(tupleAccessStack.isEmpty)

      if(arrayAccessStack.isEmpty)
        VarRef(memVar, arrayIndex=Some(ArithExpression(0)))
      else
      */

      case ViewTupleComponent(i, iv, _) =>
        val newTupleAccessStack = i :: tupleAccessStack
        generateArrayAccess(iv, arrayAccessStack, newTupleAccessStack)

      case ViewTuple(ivs, _) =>
        val i :: newTupleAccssStack = tupleAccessStack
        generateArrayAccess(ivs(i), arrayAccessStack, newTupleAccssStack)

      case ViewZip(iv, _) =>
        generateArrayAccess(iv, arrayAccessStack, tupleAccessStack)

      case _:ViewJoin =>
        IntConstant(0)
      case _:ViewMap =>
        IntConstant(0)

    }
  }

  def apply(view: View) : ExpressionT = {

    generateArrayAccess(view, List(), List())

  }

}
