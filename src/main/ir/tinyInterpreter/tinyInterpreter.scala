package ir.tinyInterpreter


import ir.TypeException
import ir.ast._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection.immutable.Map


/**
  * Created by potato on 15/07/13.
  */
object tinyInterpreter {

    type ParamToValueMap = scala.collection.immutable.Map[Param,Any]

    def apply(f: Lambda) = new {
      private def argsArrayToVector(argsWithArrays: Seq[Any]): Seq[Any] =
        argsWithArrays.map(arrayToVector)

      private def arrayToVector(arg: Any): Any = {
        arg match {
          case a: Array[_] => a.map(arrayToVector).toVector : Vector[_]
          case a: Any => a
        }
      }
      //visitNodes(f)
      /*
      def run(args: Any*): Vector[Float] = {
        val res = visitLambda(f,Map[Param,Any](),argsArrayToVector(args): _*).asInstanceOf[Vector[Float]]
        res
      }*/
      def ->[R] = new {
        def run(args: Any*): R = {
          val res = visitLambda(f,Map[Param, Any](), argsArrayToVector(args): _*).asInstanceOf[R]
          res
        }
      }
    }

    def visitLambda(l:Lambda,ParamToValue:ParamToValueMap,args:Any*): Any={
      val updatedMap = (l.params zip args).foldLeft(ParamToValue)((m,kv) => m updated (kv._1,kv._2))
      visitExpr(l.body,updatedMap)
    }
    def visitFunCall(fc:FunCall,ParamToValue:ParamToValueMap): Any={
      val argValues = fc.args.map(visitExpr(_,ParamToValue))
      fc.f match{
        case l: Lambda => visitLambda(l,ParamToValue,argValues:_*)
        case p: Pattern => visitPattern(p,ParamToValue,argValues:_*)
        case uf: UserFun => visitUserFun(uf,ParamToValue,argValues:_*)
      }
    }

    def visitExpr(e:Expr,ParamToValue:ParamToValueMap): Any = {

      e match{
        case fc:FunCall =>
          visitFunCall(fc,ParamToValue)
        case v:Value =>
          //need to implement more types
          val res = v.value.toString().toFloat
          return res
        case p:Param =>
          ParamToValue get p match{
            case Some(x) => x
            case None => throw new Error("This expression is not evaluable")

          }
      }
    }

    def visitPattern(p:Pattern,ParamToValue:ParamToValueMap,args:Any*):Any={
      p match{
        case sp:Split => visitSplit(sp,ParamToValue,args:_*)
        case m:AbstractMap => visitMap(m,ParamToValue,args:_*)
        case red:AbstractPartRed => visitRed(red,ParamToValue,args:_*)
        case j:Join => visitJoin(j,ParamToValue,args:_*)
        case _=>
          throw new NotImplementedException()
      }
    }
    def visitJoin(j:Join,ParamToValue:ParamToValueMap,args:Any*):Vector[_]={
      args.head match{
        case v: Vector[Vector[_] @unchecked] => v.flatten
      }
    }
    //Notes: More types should be implemented!!!
    def visitUserFun(uf:UserFun,ParamToValue:ParamToValueMap,args:Any*):Float={
      uf.name match{
        case "add" =>
          val arg1 = args(0) match{
            case f:Float => f
          }
          val arg2 = args(1) match{
            case f:Float => f
          }
          arg1+ arg2

        case "id" =>
          val testarg = args(0)
          val arg1 = args(0) match{
            case f:Float => f
          }
          arg1

        case "mult" =>
          val arg1 = args(0) match{
            case f:Float => f
          }
          val arg2 = args(1) match{
            case f:Float => f
          }
          arg1* arg2
      }

    }
    def visitSplit(sp:Split,ParamToValue:ParamToValueMap,args:Any*):Vector[Vector[_]]={
      args.head match{
        case v: Vector[_] =>
          v.grouped(sp.chunkSize.eval).toVector
          //something to deal with
      }
    }
    def visitMap(m:AbstractMap,ParamToValue:ParamToValueMap,args:Any*):Vector[_]={
      args.head match{
        case v: Vector[_] =>
          v.map( visitLambda(m.f,ParamToValue,_))
      }
    }
    def visitRed(red:AbstractPartRed,ParamToValue:ParamToValueMap,args:Any*): Vector[_] ={
      val init = args.head
      val input = args(1) match{
        case v :Vector[_] => v
      }
      Vector( input.foldLeft(init) ( (acc,x) => visitLambda(red.f,ParamToValue,acc,x)  ))
    }
}
