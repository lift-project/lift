package rewriting.macrorules

import ir.ast._
import opencl.ir.pattern._
import rewriting.rules.Rule

object MapSeqSlideRewrite {


  val mapSeqSlide2DSeq =

    Rule("MapSeq(MapSeq(fun(m => userFun(m)))) o Map(Transpose()) o Slide(n,s) o Map(Slide(n,s) =>"+
         "MapSeq(MapSeqSlide(fun(m => userFun(m)))) o Slide(n,s)", {
      case FunCall(MapSeq(Lambda1(Array(_), FunCall(MapSeq(f), _))),
      FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _), _)),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), slideArg), _)), arg)))) if ( n == n2 ) && ( s == s2 ) =>
      MapSeq(toGlobal(MapSeqSlide(f,n,s)) o Transpose()) o Slide(n,s) $ arg

    })

  val mapSeqSlide2D =

    Rule("Map(Map(fun(m => userFun(m)))) o Map(Transpose()) o Slide(n,s) o Map(Slide(n,s) =>"+
      "Map(MapSeqSlide(fun(m => userFun(m)))) o Slide(n,s)", {
      case FunCall(Map(Lambda(Array(_), FunCall(MapSeq(f), _), _)),
      FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _), _)),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), slideArg), _)), arg)))) if ( n == n2 ) && ( s == s2 ) =>
      Map(toGlobal(MapSeqSlide(f,n,s)) o Transpose()) o Slide(n,s) $ arg

    })

  val mapSeqSlide3DSlideNDSeq =

    Rule("MapSeq(MapSeq(MapSeq(fun(m => userFun(m))))) o " +
         "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o Slide(n,s) o Map(Slide(n,s)) o " +
         "Map(Map(Slide(n,s))) => " +
         "TransposeW() o Map(TransposeW()) o TransposeW() o MapSeq(MapSeq(MapSeqSlide(fun(m => userFun(m))))) o Transpose() o Slide2D(n,s) o Map(Transpose()) o Transpose()  ", {
    case FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(f) , _))), _))),
         FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)), _), _)),
         FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)),
         FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)), _), _)), _), _)),
         FunCall(Slide(n,s),
         FunCall(Map(Lambda(_, FunCall(Slide(n1,s1), _), _)),
         FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Slide(n2,s2), _), _)), _), _)), arg)))))))  if ( n == n1 ) && ( s == s1 ) && ( n == n2 ) && ( s == s2 ) =>
         TransposeW() o Map(TransposeW()) o TransposeW() o MapSeq(MapSeq(toGlobal(MapSeqSlide(f,n,s)) o Transpose() o Map(Transpose()))) o Transpose() o Slide2D(n,s) o Map(Transpose()) o Transpose() $ arg

  })

  val mapSeqSlide3DSlideND =

     Rule("Map(Map(MapSeq(fun(m => userFun(m))))) o " +
          "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o Slide(n,s) o Map(Slide(n,s)) o " +
          "Map(Map(Slide(n,s))) => " +
          "TransposeW() o Map(TransposeW()) o TransposeW() o Map(Map(MapSeqSlide(fun(m => userFun(m))))) o Transpose() o Slide2D(n,s) o Map(Transpose()) o Transpose()  ", {

       case FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(MapSeq(f) , _), _)), _), _)),
       FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)), _), _)),
       FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)),
       FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _), _)), _), _)), _), _)),
       FunCall(Slide(n,s),
       FunCall(Map(Lambda(_, FunCall(Slide(n1,s1), _), _)),
       FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Slide(n2,s2), _), _)), _), _)), arg)))))))  if ( n == n1 ) && ( s == s1 ) && ( n == n2 ) && ( s == s2 ) =>
       TransposeW() o Map(TransposeW()) o TransposeW() o Map(Map(toGlobal(MapSeqSlide(f,n,s)) o Transpose() o Map(Transpose()))) o Transpose() o Slide2D(n,s) o Map(Transpose()) o Transpose() $ arg

    })
}
