package ir.codeGenerator

import collection.mutable.ArrayBuffer
import ir.{ArrayType, TupleType, Type, TypeException,ScalarType}
import ir.ast._
/**
  * Created by potato on 24/07/16.
  */
object codeGenerator {
  type ParamList = ArrayBuffer[Param]
  def generateLambda(outputType:Type,currDepth:Int,maxDepth:Int): (Lambda) = {
    val (body,pl) = generateFunCall(outputType,currDepth+1,maxDepth)
    //TODO: the params in pl witch have same types may share the same one!(They are used multiple times)
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

        //Reduce
        case 5 =>
          //here we use a Join o Reduce() to fit all array cases
          //TODO: we can also use a map to fit single elements
          outputType match{
            case ArrayType(_,n) =>
              choiceNum = randChoice
              true
            case _ => false
          }

        //UserFun
        //add
        case 6 =>
          outputType match{
            case s:ScalarType =>
              choiceNum = randChoice
              true
            case _=>false

          }


        case _ => false
      }
    }

    //2. create FUnCall.f and args
    choiceNum match{

      //Join
      case 0 =>
        //generate f
        val (f,pl_f) = generateJoin(outputType)
        val argNum = f.arity
        val argType = f.revCheckType(outputType,true)

        //generate param
        if(currDepth<maxDepth) {
          val randChoice = util.Random.nextInt(2)
          val (arg, pl_arg) = randChoice match {
            case 0 => generateParam(argType)
            case 1 => generateFunCall(argType, currDepth + 1, maxDepth)
          }
          return (new FunCall(f, arg), pl_f ++= pl_arg)
        }
        else{
          val (arg,pl_arg) = generateParam(argType)
          return (new FunCall(f, arg), pl_f ++= pl_arg)
        }

      //Split
      case 1 =>
        //generate f
        val (f,pl_f) = generateSplit(outputType)
        val argNum = f.arity
        val argType = f.revCheckType(outputType,true)

        //generate param
        if(currDepth < maxDepth) {
          val randChoice = util.Random.nextInt(2)
          val (arg, pl_arg) = randChoice match {
            case 0 => generateParam(argType)
            case 1 => generateFunCall(argType, currDepth + 1, maxDepth)
          }
          return (new FunCall(f, arg), pl_f ++= pl_arg)
        }
        else{
          val (arg,pl_arg) = generateParam(argType)
          return (new FunCall(f, arg), pl_f ++= pl_arg)
        }

      //Lambda
      case 2 =>
        //generate f
        val f = generateLambda(outputType,currDepth+1,maxDepth)
        val argNum = f.params.length
        val pl_f = ArrayBuffer[Param]()

        //generate arg
        val arg = ArrayBuffer[Expr]()
        for(i <- 0 until argNum)
        {
          if(currDepth<maxDepth){
            val randChoice = util.Random.nextInt(2)
            val (arg_t,pl_arg) = randChoice match{
              case 0 => generateParam(f.params(i).t)
              case 1 => generateFunCall(f.params(i).t,currDepth+1,maxDepth)
            }
            pl_f ++= pl_arg
            arg += arg_t
          }
          else{
            val (arg_t,pl_arg) = generateParam(f.params(i).t)
            pl_f ++= pl_arg
            arg += arg_t
          }

        }
        return (new FunCall(f,arg: _*),pl_f)

      //Map
      case 3 =>
        //generate f
        val (f,pl_f) = generateMap(outputType,currDepth+1,maxDepth)

        //generate arg
        val argType = f.revCheckType(outputType,true)
        if(currDepth<maxDepth) {
          val randChoice = util.Random.nextInt(2)
          val (arg, pl_arg) = randChoice match {
            case 0 => generateParam(argType)
            case 1 => generateFunCall(argType, currDepth + 1, maxDepth)
          }
          return (new FunCall(f, arg), pl_f ++= pl_arg)
        }
        else {
          val (arg,pl_arg) = generateParam(argType)
          return (new FunCall(f,arg),pl_f ++= pl_arg)
        }

      //Id
      case 4 =>
        //generate f
        val (f,pl_f) = generateId(outputType)

        //generate arg
        val argType = f.revCheckType(outputType,true)
        if(currDepth<maxDepth) {
          val randChoice = util.Random.nextInt(2)
          val (arg, pl_arg) = randChoice match {
            case 0 => generateParam(argType)
            case 1 => generateFunCall(argType, currDepth + 1, maxDepth)
          }
          return (new FunCall(f, arg), pl_f ++= pl_arg)
        }
        else{
          val (arg,pl_arg) = generateParam(argType)
          return (new FunCall(f,arg),pl_f ++= pl_arg)
        }

      //Reduce
      case 5 =>
        outputType match{
          case ArrayType(t,n) =>
            //generate f
            val reduceOutType = ArrayType(ArrayType(t,n),1)
            val (f,pl_f) = generateRed(reduceOutType,currDepth+1,maxDepth)

            //generate args
            val arg = ArrayBuffer[Expr]()
            val argType = f.revCheckType(reduceOutType,true)
            argType match{
              case a:TupleType =>
                for(i <- 0 until f.f.params.length){
                  if(currDepth<maxDepth){
                    val randChoice = util.Random.nextInt(2)
                    val (arg_t,pl_arg) = randChoice match{
                      case 0 => generateParam(a.elemsT(i))
                      case 1 => generateFunCall(a.elemsT(i),currDepth+1,maxDepth)
                    }
                    pl_f ++= pl_arg
                    arg += arg_t
                  }
                  else{
                    val (arg_t,pl_arg) = generateParam(a.elemsT(i))
                    pl_f ++= pl_arg
                    arg += arg_t
                  }
                }
                return (new FunCall(new Join,FunCall(f,arg:_*)),pl_f)
              case _ =>
                throw new TypeException(argType,"TupleType")

            }


          //TODO: we can also use map to fit single elements!
          case _ =>
            throw new TypeException(outputType,"ArrayType(t,n)")
        }
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
  def matchParamType(p:ParamList,T:Type):Int ={
    for(i <- 0 until p.length) {
      p(i).t match {
        case T =>
          i
        case _ =>
      }
    }
    -1
  }
  def generateRed(outputType:Type,currDepth:Int,maxDepth:Int):(Reduce,ParamList) = {
    val reduceOutType = outputType match {
      case ArrayType(t, n) =>
        if (n.eval == 1)
          t
        else
          throw new TypeException(outputType, "ArrayType(t,1)")
      case _ =>
        throw new TypeException(outputType, "ArrayType(t,1)")
    }
    val forceToUseAllParam = false
    if (forceToUseAllParam) {
      val failMax = 1024
      var failCount = 0
      do {
        val (body, pl) = generateFunCall(reduceOutType, currDepth + 1, maxDepth)
        val initId = matchParamType(pl, reduceOutType)
        if (pl.length >= 2 && initId >= 0) {
          val param_init = pl.remove(initId)
          val randChoice = util.Random.nextInt(pl.length)
          val param_array = pl.remove(randChoice)
          return (Reduce(Lambda(Array(param_init, param_array), body)), pl)
        }
        else {
          failCount += 1
        }

      } while (failCount < failMax)
    }
    val (body, pl) = generateFunCall(reduceOutType, currDepth + 1, maxDepth)
    if (pl.length >= 2) {
      val initId = matchParamType(pl, reduceOutType)
      if (initId >= 0) {
        val param_init = pl.remove(initId)
        val randChoice = util.Random.nextInt(pl.length)
        val param_array = pl.remove(randChoice)
        return (Reduce(Lambda(Array(param_init, param_array), body)), pl)
      }
    }

    val (param_init, _) = generateParam(reduceOutType)
    val randChoice = util.Random.nextInt(pl.length)
    val param_array = pl.remove(randChoice)
    return (Reduce(Lambda(Array(param_init, param_array), body)), pl)
  }
  //}
}
