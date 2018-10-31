package rewriting.macrorules

import ir.ast._
import opencl.ir.pattern.{MapSeq, MapSeqSlide}
import rewriting.rules.Rule

object MapSeqSlideRewrite {


  val mapSeqSlide2DSeq =

    Rule("Map(Map(fun(m => userFun(m)))) o Map(Transpose()) o Slide(n,s) o Map(Slide(n,s) =>"+
         "Map(MapSeqSlide(fun(m => userFun(m)))) o Slide(n,s)", {
      case FunCall(MapSeq(Lambda1(Array(_), FunCall(MapSeq(f), _))),
      FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _))),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), slideArg))), arg)))) =>
        // TODO: add if n==n2 && s==s2
          MapSeq(MapSeqSlide(f,n,s)) o Slide(n,s) $ arg

    })

  val mapSeqSlide2D =

    Rule("Map(Map(fun(m => userFun(m)))) o Map(Transpose()) o Slide(n,s) o Map(Slide(n,s) =>"+
      "Map(MapSeqSlide(fun(m => userFun(m)))) o Slide(n,s)", {
      case FunCall(Map(Lambda(Array(_), FunCall(MapSeq(f), _))),
      FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _))),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), slideArg))), arg)))) =>
        MapSeq(MapSeqSlide(f,n,s)) o Slide(n,s) $ arg

    })

  /*
    Map(Map(Map(fun(m => userFun(m))))) o Map(Map(Transpose()) o Transpose()) o
          Slide(a, b) o Map(Map(Transpose()) o Slide(a, b) o Map(Slide(a, b))) =>
    Map(Map( fun(x => {
          MapSeqSlide( fun(m => jacobi(m)), a,b)
        } o Transpose() o Map(Transpose()) $ x ))) o
          Map(Transpose()) o Slide(a, b) o Map(Slide(a, b))
   */

  // TODO: fix the parentheses here

  val mapSeqSlide3DSeq =

    Rule("Map(Map(Map(fun(m => userFun(m))))) o Map(Map(Transpose()) o Transpose()) o "+
      "Slide(a, b) o Map(Map(Transpose()) o Slide(a, b) o Map(Slide(a, b))) =>"+
      "Map(Map( fun(x => {"+
      "MapSeqSlide( fun(m => jacobi(m)), a,b)"+
      "} o Transpose() o Map(Transpose()) $ x ))) o"+
      "Map(Transpose()) o Slide(a, b) o Map(Slide(a, b))", {
      /*   {
           case FunCall(MapSeq(lambda), FunCall(Slide(n,s), arg)) =>
             MapSeqSlide(lambda,n,s) $ arg
         })*/
      case FunCall(MapSeq(Lambda1(Array(_), FunCall(MapSeq(Lambda1(Array(_), FunCall(MapSeq(f),_)))))),
      FunCall(Map(Lambda(Array(_), FunCall(Map(Lambda(Array(_), FunCall(Transpose(),_))), FunCall(Transpose(),_)))),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_),FunCall(Map(Lambda(Array(_), FunCall(Transpose(),_))),
      FunCall(Slide(n2,s2), FunCall(Map(Lambda(Array(_), FunCall(Slide(n3,s3), _)))))))), arg)))) =>
        // TODO: add if n==n2 && s==s2
        MapSeq(MapSeq(MapSeqSlide(f,n,s))) o Slide(n,s) $ arg

    })

  val mapSeqSlide3D =

     Rule("Map(Map(Map(fun(m => userFun(m))))) o Map(Map(Transpose()) o Transpose()) o "+
          "Slide(a, b) o Map(Map(Transpose()) o Slide(a, b) o Map(Slide(a, b))) =>"+
          "Map(Map( fun(x => {"+
          "MapSeqSlide( fun(m => jacobi(m)), a,b)"+
          "} o Transpose() o Map(Transpose()) $ x ))) o"+
          "Map(Transpose()) o Slide(a, b) o Map(Slide(a, b))", {
    /*   {
         case FunCall(MapSeq(lambda), FunCall(Slide(n,s), arg)) =>
           MapSeqSlide(lambda,n,s) $ arg
       })*/
      case FunCall(Map(Lambda(Array(_), FunCall(Map(Lambda(Array(_), FunCall(MapSeq(f),_)))))),
           FunCall(Map(Lambda(Array(_), FunCall(Map(Lambda(Array(_), FunCall(Transpose(),_)))))),
           FunCall(Slide(n,s),
           FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), _))), arg)))) =>
           // TODO: add if n==n2 && s==s2
           Map(Map(MapSeqSlide(f,n,s))) o Slide(n,s) $ arg

    })
}
