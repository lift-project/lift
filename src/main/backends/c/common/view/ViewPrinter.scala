package backends.c.common.view

import core.generator.GenericAST
import core.generator.GenericAST.{ArithExpression, ExpressionT, IntConstant, StringConstant, VarRef}
import ir.{ArrayType, ArrayTypeWS, Size}
import ir.view._
import lift.arithmetic.{ArithExpr, Cst}

object ViewPrinter {


  def generateArrayAccess(the_view: View,
                          arrayAccessStack: List[ArithExpr],
                          tupleAccessStack: List[Int]): ExpressionT = {
    the_view match {

      case ViewAccess(i, iv, _) =>
        generateArrayAccess(iv, i::arrayAccessStack, tupleAccessStack)

      case ViewSplit(chunksize, iv, _) =>
        val chunkIdx :: elemIdx :: rest = arrayAccessStack
        val newIdx = chunkIdx * chunksize + elemIdx
        generateArrayAccess(iv, newIdx::rest, tupleAccessStack)

      case ViewConstant(value, _) =>
        GenericAST.RawCode(value.value)

      case current_view@ViewMem(memVar, ty) =>
        tupleAccessStack.isEmpty match {
          case true =>
            assert(arrayAccessStack.size == 1);VarRef(memVar, arrayIndex = Some( ArithExpression(arrayAccessStack.head) ))
          case false =>
            //assert(false, "Not implement"); IntConstant(0)
            val tuple_id :: rest = tupleAccessStack
            val array_access_var = generateArrayAccess(current_view, arrayAccessStack  , rest ).asInstanceOf[VarRef]

            array_access_var.suffix match {
              case None =>
                VarRef(  array_access_var.v, Some("._" + tuple_id), array_access_var.arrayIndex )
              case Some(existed_suffix) =>
                VarRef(  array_access_var.v, Some(existed_suffix + "._" + tuple_id), array_access_var.arrayIndex )
            }

        }

        /*
      case ViewMemWithInnerView(memVar, iv, ty) =>
        tupleAccessStack.isEmpty match {
          case false => assert(tupleAccessStack.size == 1);
            val varRef = VarRef(memVar, arrayIndex = Some( ArithExpression(arrayAccessStack.head) ))
            generateArrayAccess(iv, arrayAccessStack, tupleAccessStack, varRef :: varRefStack )
          case true => assert(false, "Not implement"); IntConstant(0)
        }
      case ViewMemScalar(i, ty) =>
        assert(varRefStack.length == 1)
        val varRef :: Nil = varRefStack
        VarRef(varRef.v, Some("._"+i) ,varRef.arrayIndex)
*/
      case ViewTupleComponent(i, iv, _) =>
        val newTupleAccessStack = i :: tupleAccessStack
        generateArrayAccess(iv, arrayAccessStack, newTupleAccessStack)

      case ViewTuple(ivs, _) =>
        val i :: newTupleAccssStack = tupleAccessStack
        generateArrayAccess(ivs(i), arrayAccessStack, newTupleAccssStack)

      case ViewZip(iv, _) =>
        generateArrayAccess(iv, arrayAccessStack, tupleAccessStack)

      case ViewReorder(reindexFun, iv, _) =>
        val idx :: indices = arrayAccessStack
        generateArrayAccess(iv, reindexFun(idx) :: indices, tupleAccessStack)

      case ViewTranspose(iv, _) =>
        val top :: second :: rest = arrayAccessStack
        generateArrayAccess(iv, second::top::rest, tupleAccessStack)

      case ViewPad(iv, left, _, padFun, _) =>
        val idx :: indices = arrayAccessStack
        val currentIdx = idx - left
        val length = iv.t.asInstanceOf[ArrayType with Size].size
        val newIdx = if(ArithExpr.mightBeNegative(currentIdx) || ArithExpr.isSmaller(length -1, currentIdx.max).getOrElse(true))
          padFun(currentIdx, length)
        else
          currentIdx
        generateArrayAccess(iv, newIdx :: indices, tupleAccessStack)


      case ViewGeneratorUserFun(f, ArrayTypeWS(_, m)) =>
        assert(arrayAccessStack.length == 1)
        val i :: Nil = arrayAccessStack
        GenericAST.FunctionCall(
          f.name,
          List(i, m).map(ArithExpression(_))
            /*
          List(i, m)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression) */
        )

      case View3DGeneratorUserFun(f, ArrayTypeWS(ArrayTypeWS(ArrayTypeWS(_, o), n), m)) =>
        val i :: j :: k :: _ = arrayAccessStack
        GenericAST.FunctionCall(
          f.name,
          List(i, j, k, m, n, o).map(ArithExpression(_))
          /*
          List(i, j, k, m, n, o)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression)
            */
        )

      case ViewJoin(chunkSize, iv, _) =>
        val idx :: indices = arrayAccessStack
        val chunkIdx = idx / chunkSize
        val elemIdx = idx % chunkSize
        generateArrayAccess(iv, chunkIdx :: elemIdx :: indices, tupleAccessStack)
        //generateArrayAccess(iv,  elemIdx :: chunkIdx ::indices, tupleAccessStack)

      case ViewMapSeq(iv, itVar, _ ) =>
        val idx :: indices = arrayAccessStack
        generateArrayAccess(iv, indices, tupleAccessStack)
        //val newV = iv.replaced(itVar, idx)
        //generateArrayAccess(newV, indices, tupleAccessStack)


      case ViewMap(iv, itVar, _) =>
        val idx :: indices = arrayAccessStack
        val newV = iv.replaced(itVar, idx)
        generateArrayAccess(newV, indices, tupleAccessStack)

      case ViewSlide(iv, slide, _) =>
        val chunkIdx :: elemIdx :: indices = arrayAccessStack
        val newIdx = chunkIdx * slide.step + elemIdx
        generateArrayAccess(iv, newIdx :: indices, tupleAccessStack)

      case ViewSlice(iv, slice, _) =>
        assert( arrayAccessStack.length > 0 )
        val oldIdx :: indices = arrayAccessStack
        val newIdx = oldIdx + slice.startIdx
        generateArrayAccess(iv, newIdx :: indices, tupleAccessStack)


      case _ =>
        assert(false, "Pattern may not be implemented in view printer"); StringConstant("Unreachable!")

    }
  }

  def apply(view: View) : ExpressionT = {

    generateArrayAccess(view, List(), List())

  }

}
