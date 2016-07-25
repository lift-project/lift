package ir.codeGenerator

import collection.mutable.ArrayBuffer
import ir.{ArrayType, Type}
import ir.ast._
/**
  * Created by potato on 24/07/16.
  */
object codeGenerator {
  type ParamList = ArrayBuffer[Param]
  def generateLambda(outputType:Type): (Lambda) = {
    val (body,pl) = generateFunCall(outputType)
    return Lambda(pl.toArray[Param],body)

  }
  def generateFunCall(outputType:Type):(FunCall,ParamList)={
    val totChoiceNum = 3
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
              choiceNum = randChoice
              true
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
          case 1 => generateFunCall(argType)
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
          case 1 => generateFunCall(argType)
        }
        return (new FunCall(f,arg),pl_f++=pl_arg)

      //Lambda
      case 2 =>
        val f = generateLambda(outputType)
        val argNum = f.params.length
        val pl_f = ArrayBuffer[Param]()
        val arg = ArrayBuffer[Expr]()
        for(i <- 0 until argNum)
        {
          val randChoice = util.Random.nextInt(2)
          val (arg_t,pl_arg) = randChoice match{
            case 0 => generateParam(f.params(i).t)
            case 1 => generateFunCall(f.params(i).t)
          }
          pl_f ++= pl_arg
          arg += arg_t
        }
        return (new FunCall(f,arg: _*),pl_f)
        


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
}
