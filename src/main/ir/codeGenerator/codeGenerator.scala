package ir.codeGenerator

import collection.mutable.ArrayBuffer
import ir.{ArrayType, Type, TypeException}
import ir.ast._
/**
  * Created by potato on 24/07/16.
  */
object codeGenerator {
  type ParamList = ArrayBuffer[Param]
  def generateLambda(outputType:Type,currDepth:Int,maxDepth:Int): (Lambda) = {
    val (body,pl) = generateFunCall(outputType,currDepth+1,maxDepth)
    return Lambda(pl.toArray[Param],body)

  }
  def generateFunCall(outputType:Type,currDepth:Int,maxDepth:Int):(FunCall,ParamList)={
    val totChoiceNum = 5
    var matched = false
    var choiceNum = 0
    //1.check which FunDecl could be used
    while(matched != true) {
      val randChoice = util.Random.nextInt(totChoiceNum)
      matched = randChoice match{

        //Join
        case 0 =>
          outputType match {
            case ArrayType(t, n) =>
              if(n.eval < 4) {
                  false
                }
              else {
                choiceNum = randChoice
                true
              }
            case _ => false
          }

        //Split
        case 1 =>
          outputType match{
            case ArrayType(ArrayType(t,m),n) =>
              choiceNum = randChoice
              true
            case _ => false
          }

        //Lambda
        case 2 =>
          choiceNum = randChoice
          //true
          false

        //Map
        case 3 =>
          outputType match{
            case ArrayType(t,n) =>
              choiceNum = randChoice
              true
            case _ => false
          }

        //Id
        case 4 =>
          choiceNum = randChoice
          true

        case _ => false
      }
    }

    //2. create FUnCall.f and args
    choiceNum match{

      //Join
      case 0 =>
        val (f,pl_f) = generateJoin(outputType)
        val argNum = f.arity
        val argType = f.revCheckType(outputType,true)
        val randChoice = util.Random.nextInt(2)
        val (arg,pl_arg) = randChoice match{
          case 0 => generateParam(argType)
          case 1 => generateFunCall(argType,currDepth+1,maxDepth)
        }
        return (new FunCall(f,arg),pl_f++=pl_arg)

      //Split
      case 1 =>
        val (f,pl_f) = generateSplit(outputType)
        val argNum = f.arity
        val argType = f.revCheckType(outputType,true)
        val randChoice = util.Random.nextInt(2)
        val (arg,pl_arg) = randChoice match{
          case 0 => generateParam(argType)
          case 1 => generateFunCall(argType,currDepth+1,maxDepth)
        }
        return (new FunCall(f,arg),pl_f++=pl_arg)

      //Lambda
      case 2 =>
        val f = generateLambda(outputType,currDepth+1,maxDepth)
        val argNum = f.params.length
        val pl_f = ArrayBuffer[Param]()
        val arg = ArrayBuffer[Expr]()
        for(i <- 0 until argNum)
        {
          val randChoice = util.Random.nextInt(2)
          val (arg_t,pl_arg) = randChoice match{
            case 0 => generateParam(f.params(i).t)
            case 1 => generateFunCall(f.params(i).t,currDepth+1,maxDepth)
          }
          pl_f ++= pl_arg
          arg += arg_t
        }
        return (new FunCall(f,arg: _*),pl_f)

      //Map
      case 3 =>
        val (f,pl_f) = generateMap(outputType,currDepth+1,maxDepth)
        val argType = f.revCheckType(outputType,true)

        val randChoice = util.Random.nextInt(2)
        val (arg,pl_arg) = randChoice match{
          case 0 => generateParam(argType)
          case 1 => generateFunCall(argType,currDepth+1,maxDepth)
        }
        return (new FunCall(f,arg),pl_f++=pl_arg)

      //Id
      case 4 =>
        val (f,pl_f) = generateId(outputType)
        val argType = f.revCheckType(outputType,true)
        val randChoice = util.Random.nextInt(2)
        val (arg,pl_arg) = randChoice match{
          case 0 => generateParam(argType)
          case 1 => generateFunCall(argType,currDepth+1,maxDepth)
        }
        return (new FunCall(f,arg),pl_f++=pl_arg)


        


    }

  }
  def generateParam(outputType:Type):(Param,ParamList)={
    val p = Param(outputType)
    val pl = ArrayBuffer(p)
    return (p,pl)

  }
  def generateJoin(outputType:Type):(Join,ParamList) ={
    return(new Join,ArrayBuffer[Param]())
  }
  def generateSplit(outputType:Type):(Split,ParamList) ={
    return(new Split(4),ArrayBuffer[Param]())
  }
  def generateMap(outputType:Type,currDepth:Int,maxDepth:Int):(Map,ParamList) = {
    val mapOutType = outputType match{
      case ArrayType(t,n) =>
        t
      case _=>
        throw new TypeException(outputType,"ArrayType")
    }
    val (body,pl) = generateFunCall(mapOutType,currDepth+1,maxDepth)
    val randChoice = util.Random.nextInt(pl.length)
    val arg = pl.remove(randChoice)
    val arg_array = Array(arg)
    return (new Map(Lambda(arg_array,body)),pl)

  }
  def generateId(outputType:Type): (Id,ParamList) =
  {
    return (new Id,ArrayBuffer[Param]())
  }
  //def generateReduce(outputType:Type,currDepth:Int,maxDepth:Int):(Reduce,ParamList) = {
    //val (body,pl) = generateFunCall(outputType,currDepth+1,maxDepth)

  //}
}
