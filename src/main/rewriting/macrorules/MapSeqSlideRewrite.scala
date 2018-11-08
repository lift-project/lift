package rewriting.macrorules

import ir.ast._
import opencl.ir.pattern.{MapSeq, MapSeqSlide, toGlobal}
import rewriting.rules.Rule

object MapSeqSlideRewrite {


  val mapSeqSlide2DSeq =

    Rule("Map(Map(fun(m => userFun(m)))) o Map(Transpose()) o Slide(n,s) o Map(Slide(n,s) =>"+
         "Map(MapSeqSlide(fun(m => userFun(m)))) o Slide(n,s)", {
      case FunCall(MapSeq(Lambda1(Array(_), FunCall(MapSeq(f), _))),
      FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _))),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), slideArg))), arg)))) if ( n == n2 ) && ( s == s2 ) =>
        // TODO: add if n==n2 && s==s2
          MapSeq(toGlobal(MapSeqSlide(f,n,s)) o Transpose()) o Slide(n,s) $ arg
        // what it should be ...
        // MapSeq(MapSeqSlide(f,n,s)) o Slide(n,s) $ arg

    })

  val mapSeqSlide2D =

    Rule("Map(Map(fun(m => userFun(m)))) o Map(Transpose()) o Slide(n,s) o Map(Slide(n,s) =>"+
      "Map(MapSeqSlide(fun(m => userFun(m)))) o Slide(n,s)", {
      case FunCall(Map(Lambda(Array(_), FunCall(MapSeq(f), _))),
      FunCall(Map(Lambda(Array(_), FunCall(Transpose(), _))),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), slideArg))), arg)))) if ( n == n2 ) && ( s == s2 ) =>
        Map(MapSeqSlide(f,n,s)) o Slide(n,s) $ arg

    })



  // TODO : Fix? This does not work!
  // but, will never match with "Slide3D" anyway because of differences between the SlideND version
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
      case FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(f),_))), _))),
      FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Transpose(),_)))),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(Array(_),FunCall(Map(Lambda(Array(_), FunCall(Transpose(),_))),
      FunCall(Slide(n2,s2), FunCall(Map(Lambda(Array(_), FunCall(Slide(n3,s3), _))),_),_)))), arg)))) if ( n == n2 ) && ( s == s2 ) && ( n == n3 ) && ( s == s3) =>
        // TODO: add if n==n2 && s==s2
        MapSeq(MapSeq(MapSeqSlide(f,n,s))) o Slide(n,s) $ arg

    })

  val mapSeqSlide3DSlideNDSeq =

    Rule("MapSeq(MapSeq(MapSeq(fun(m => userFun(m))))) o " +
      "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o Slide(n,s) o Map(Slide(n,s)) o " +
      "Map(Map(Slide(n,s))) => " +
      "MapSeq(MapSeq(MapSeqSlide(fun(m => userFun(m))))) o Slide2D(n,s)  ", {
    case FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(f) , _))), _))),
         FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))),
         FunCall(Map(Lambda(_, FunCall(Transpose(), _))),
         FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), _))),
         FunCall(Slide(n,s),
         FunCall(Map(Lambda(_, FunCall(Slide(n1,s1), _))),
         FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Slide(n2,s2), _))), _))), arg)))))))  if ( n == n1 ) && ( s == s1 ) && ( n == n2 ) && ( s == s2 ) =>
         MapSeq(MapSeq(toGlobal(MapSeqSlide(f,n,s)) o Transpose() o Map(Transpose()))) o Slide2D(n,s) $ arg
         // what it should be ...
         //MapSeq(MapSeq(MapSeqSlide(f,n,s))) o Slide2D(n,s) $ arg
  })


  val mapSeqSlide3DSlideNDTwoZippedSeqZ =

    Rule("MapSeq(MapSeq(MapSeq(fun(m => userFun(m))))) o " +
      "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o Slide(n,s) o Map(Slide(n,s)) o " +
      "Map(Map(Slide(n,s))) => " +
      "MapSeq(MapSeq(MapSeqSlide(fun(m => userFun(m))))) o Slide2D(n,s)  ", {
      case FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(Lambda1(_, FunCall(MapSeq(f) , _))), _))),
      FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))),
      FunCall(Map(Lambda(_, FunCall(Transpose(), _))),
      FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), _))),
      FunCall(Slide(n,s),
      FunCall(Map(Lambda(_, FunCall(Slide(n1,s1), _))),
      FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Slide(n2,s2), _))), _))), arg)))))))  if ( n == n1 ) && ( s == s1 ) && ( n == n2 ) && ( s == s2 ) =>
        MapSeq(MapSeq(toGlobal(MapSeqSlide(f,n,s)) o Transpose() o Map(Transpose()))) o Slide2D(n,s) $ arg
      // what it should be ...
      //MapSeq(MapSeq(MapSeqSlide(f,n,s))) o Slide2D(n,s) $ arg
    })

  val mapSeqSlide3DSlideND =

     Rule("Map(Map(Map(fun(m => userFun(m))))) o " +
          "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o Slide(n,s) o Map(Slide(n,s)) o " +
          "Map(Map(Slide(n,s))) => " +
          "Map(Map(MapSeqSlide(fun(m => userFun(m))))) o Slide2D(n,s)  ", {

      case FunCall(Map(Lambda(Array(_), FunCall(Map(Lambda(Array(_), FunCall(MapSeq(f),_)))))),
           FunCall(Map(Lambda(Array(_), FunCall(Map(Lambda(Array(_), FunCall(Transpose(),_)))))),
           FunCall(Slide(n,s),
           FunCall(Map(Lambda(Array(_), FunCall(Slide(n2,s2), _))), arg)))) =>
           // TODO: add if n==n2 && s==s2
           Map(Map(MapSeqSlide(f,n,s))) o Slide2D(n,s) $ arg

    })
}
