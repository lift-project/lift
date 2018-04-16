import ir.ast._
import ir.ast.debug.AssertType
import ir.{ArrayTypeWSWC, TupleType, TypeChecker}
import opencl.ir.pattern.MapSeq
import opencl.ir.{Float, id}

/**
  * Created by nm on 11/04/18.
  */

object dontweAll {

  def l1: FunDecl = fun(
    ArrayTypeWSWC(TupleType(Float,Float),2), (A) => {
      MapSeq(id) $ A
    }
  )
  def l2(): FunDecl = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, 25), 2), (B) => {
      MapSeq(l1) o AssertType(ArrayTypeWSWC(ArrayTypeWSWC(Float, 25), 2)) $ B
    }
  )  

  def main(args: Array[String]): Unit = {
    println(TypeChecker(l2))
  }

}