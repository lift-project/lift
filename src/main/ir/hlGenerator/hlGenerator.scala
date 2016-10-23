package ir.hlGenerator

import java.io.PrintWriter

import collection.mutable._
import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Eval, Execute}
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import rewriting.{EnabledMappings, Lower, Rewrite, Rules}

import scala.collection.JavaConverters._
import scala.language.reflectiveCalls

class hlGenerator {

  val RefinedResult: ArrayBuffer[Lambda] = new ArrayBuffer[Lambda]()
  val ParamList: ArrayBuffer[Param] = new ArrayBuffer[Param]()
  //var FunCallList: ArrayBuffer[FunCall] = new ArrayBuffer[FunCall]()
  val LambdaList: ArrayBuffer[Lambda] = new ArrayBuffer[Lambda]()
  val ParamToFunCall = collection.mutable.Map[Param, FunCall]()
  val UnpackedToExpr = collection.mutable.Map[Param,Expr]()

  //Used for debug
  var AssignedChoiceNum = 0
  var PassCounter = 0
  val PassParamUpPossibility = 0.0
  val PrintDebugInfo = true



  //controllers for run programs
  val GlobalSize = 512
  val LocalSize = 32
  val RunInterpreter = false
  val useRandomRewrite = true
  val rewriteDepth = 3

  //controllers for generate programs
  val LoopNum = 30
  val consequentUserFun = false
  //val ReduceInitToGlobal = false
  val ReduceOnOneElement = false
  val AllowJoinASplit = false
  val MustContainsUserFun = true
  val MustContainsMap = true
  val MapStrictMatchUnpack = true
  val ReduceStrictMatchUnpack = true
  var LimitNum = 40

  //controllers for patterns
  val ZipLimit = 2
  val SplitChunkSize = 4
  val GenJoin = true
  val GenSplit = true
  val GenUserFun = true
  val GenZip = true
  val GenGet = true
  val GenMap = true
  val GenReduce = true


  //Avoid for redundant
  //Join
  var Join_P = 0

  //Split
  var Split_P = 0

  //Reduce
  val Reduce_L_PI_PE = scala.collection.mutable.Set[(Int, Int, Int)]()
  val Reduce_Lambda_Check = scala.collection.mutable.Set[Int]()

  //UserFun
  val Add_Check = scala.collection.mutable.Set[(Int, Int)]()

  //Map
  val Map_L_E = scala.collection.mutable.Set[(Int, Int)]()
  var Map_Checked = 0

  //Zip
  var Zip_P = 0

  //Get
  var Get_P = 0

  //UnpackParam
  var UnPack_P = 0


  private def tryRewriting(l:Lambda,w:PrintWriter):scala.Seq[Lambda] ={
    useRandomRewrite match {
      case true =>
        try {
          //Rewrite.rewriteWithoutLowering(l,rewriting.allRulesWithoutLowering,rewriteDepth)
          Rewrite.rewriteWithoutLowering (l, rewriting.allRulesWithoutMapsLowering (1, 1, 1), rewriteDepth)
        }
        catch {
          case e: Throwable =>
            println ("catch a exception in random-rewrite-by-user")
            writeln (w, "catch a exception in random-rewrite-by-user")

            if(PrintDebugInfo) {
              e.printStackTrace(w)
              e.printStackTrace ()
            }
            return Seq[Lambda]()
        }
      case false =>
        Seq(l)
    }
  }

  private def tryLowering(l:Lambda,w:PrintWriter):List[Lambda] ={
    try {
      Lower.mapCombinations(l, new EnabledMappings(true, true, true, true, true, true))
    }
    catch {
      case e: Throwable =>
        if(PrintDebugInfo) {
          println("catch a exception in lower-parser-by-user")
          writeln(w, "catch a exception in lower-parser-by-user")
          e.printStackTrace(w)
          e.printStackTrace ()
        }
        return List[Lambda]()
    }
  }

  private def tryCompile(l:Lambda,w:PrintWriter):(String,Boolean)={
    try {
      if(PrintDebugInfo) {
        println(l.toString)
        writeln(w, l.toString)
      }
      (Compile(l),true)
    }
    catch {
      case e: Throwable =>
        if(PrintDebugInfo) {
          println("catch a exception in compiler-by-user")
          writeln(w, "catch a exception in compiler-by-user")
        }
        e.printStackTrace(w)
        return("",false)
    }
  }
  //Testing methods
  def trySingleLambda(l: Lambda, oril: Lambda, w: PrintWriter): Unit = {
    if(l.params.length > 7){
      if(PrintDebugInfo) {
        println("too many argument, Ignored-by-user")
        writeln(w, "too many argument, Ignored-by-user")
      }
      return
    }
    //1. Generate Input Data
    val Args = scala.collection.mutable.ArrayBuffer[Any]()
    for (j <- l.params.indices) {
      l.params(j).t match {
        case ArrayType(ArrayType(Float, l1), l2) =>
          Args += Array.tabulate(l1.eval, l2.eval)((r, c) => 1.0f)
        case ArrayType(Float, l1) =>
          Args += Array.fill(l1.eval)(2.0f)
        case Float =>
          Args += 3.0f
        case _ =>
          if(PrintDebugInfo) {
            println("input type not inplemented, Ignored-by-user")
            writeln(w, "input type not inplemented, Ignored-by-user")
          }
          return
      }
    }

    //2. Pass the TypeChecker
    val outType = TypeChecker(l)

    //2.5 run the interpreter if we want
    val output_int:Array[Float] = RunInterpreter match{
      case true =>
        outType match{
          case Float =>
            Array[Float](Interpreter(oril).->[Float].run(Args: _*))
          case ArrayType(Float,d1) =>
            Interpreter(oril).->[Vector[Float]].run(Args: _*).toArray[Float]
          case ArrayType(ArrayType(Float, d1), d2) =>
            Interpreter(oril).->[Vector[Vector[Float]]].runAndFlatten(Args: _*).toArray[Float]
          case ArrayType(ArrayType(ArrayType(Float, d1), d2), d3) =>
            Interpreter(oril).->[Vector[Vector[Vector[Float]]]].runAndFlatten(Args: _*).toArray[Float]
          case _=>
            if(PrintDebugInfo) {
              println("OutPut Type unimplemented,Ignored-by-user")
              writeln(w, "OutPut Type unimplemented,Ignored-by-user")
            }
            return
        }
      case false =>
        Array[Float](0)
    }

    //3. random rewriting
    val afterRewriting:scala.Seq[Lambda] = tryRewriting(l,w)

    if(afterRewriting.length < 1){
      return
    }


    for(rewriteId <- afterRewriting.indices) {

      //3. lower the lambda
      //var fs = List[Lambda]()
      val afterLowering = tryLowering(afterRewriting(rewriteId), w)

      if (afterLowering.length < 1) {
        if (PrintDebugInfo) {
          println("No suitable lower-expressions, Ignored-by-user")
          writeln(w, "No suitable lower-expressions, Ignored-by-user")
        }
      }
      else {
        for (lowId <- afterLowering.indices) {

          val lowLevel = afterLowering(lowId)
          if (lowLevel.isGenerable) {

            //4. compile the lambda
            val (code,compileSucc) = tryCompile(lowLevel,w)
            if(compileSucc) {

              //5. execute the OpenCL kernel
              try {
                //val (output_exe: Array[Float], runtime) = Execute(LocalSize, GlobalSize)(code, lowLevel, Args: _*)
                //val (output_exe: Array[Float], runtime1) = Execute(GlobalSize, GlobalSize)(code, lowLevel, Args: _*)

                var currGlobalSize = GlobalSize
                while (currGlobalSize >= 1) {
                  var currLclSize = currGlobalSize
                  while (currLclSize >= 1) {
                    val (output_exe: Array[Float], runtime) = Execute(currLclSize, currGlobalSize)(code, lowLevel, Args: _*)
                    if (RunInterpreter) {
                      if (output_exe.corresponds(output_int)(_ == _)) {
                        writeln(w, "results eq-by-user")
                        println("results eq-by-user")
                      }
                      else {
                        writeln(w, "results ne-by-user")
                        println("results ne-by-user")
                      }
                    }
                    else {
                      println("pass-by-user")
                      println("globalsize = " + currGlobalSize + " localsize = " + currLclSize)
                      println("time = " + runtime)
                      writeln(w, "pass-by-user")
                      writeln(w, "globalsize = " + currGlobalSize + " localsize = " + currLclSize)
                      writeln(w, "time = " + runtime)
                      PassCounter += 1
                      println(PassCounter + "passed for now")
                      writeln(w, PassCounter + "passed for now")
                    }
                    currLclSize /= 4
                  }
                  currGlobalSize /= 4
                }
              }
              catch {
                case e: Throwable =>
                  println("catch a exception in execator-by-user")
                  e.printStackTrace()
                  writeln(w, "catch a exception in execator-by-user")
                  e.printStackTrace(w)
              }
            }
          }
        }
      }
    }
          /*//5. execute the OpenCL kernel and the interpreter
        outType match {
          case Float =>
            try {
              val (output_exe: Float, runtime) = Execute(LocalSize, GlobalSize)(code, lowLevel, Args: _*)
              if (RunInterpreter) {

                val output_int = Interpreter(oril).->[Float].run(Args: _*)
                if (output_exe == output_int) {
                  println("results eq-by-user")
                  writeln(w, "results eq-by-user")
                }
                else {
                  println("results ne-by-user")
                  writeln(w, "results ne-by-user")
                }
              }
              else {
                println("pass-by-user")
                writeln(w, "pass-by-user")
              }

            }
            catch {
              case e: Throwable =>
                println("catch a exception in execator-by-user")
                e.printStackTrace()
                writeln(w, "catch a exception in execator-by-user")
                e.printStackTrace(w)
                return
            }
          case ArrayType(Float, d1) =>
            try {
              val (output_exe: Array[Float], runtime) = Execute(LocalSize, GlobalSize)(code, lowLevel, Args: _*)
              if (RunInterpreter) {
                val output_int = Interpreter(oril).->[Vector[Float]].run(Args: _*).toArray[Float]
                if (output_exe.corresponds(output_int)(_ == _)) {
                  writeln(w, "results eq-by-user")
                  println("results eq-by-user")
                }
                else {
                  writeln(w, "results ne-by-user")
                  println("results ne-by-user")
                }
              }
              else {
                println("pass-by-user")
                writeln(w, "pass-by-user")
              }
            }
            catch {
              case e: Throwable =>
                println("catch a exception in execator-by-user")
                e.printStackTrace()
                writeln(w, "catch a exception in execator-by-user")
                e.printStackTrace(w)
                return
            }
          case ArrayType(ArrayType(Float, d1), d2) =>
            try {
              val (output_exe: Array[Float], runtime) = Execute(LocalSize, GlobalSize)(code, lowLevel, Args: _*)
              if (RunInterpreter) {
                val output_int = Interpreter(oril).->[Vector[Vector[Float]]].runAndFlatten(Args: _*).toArray[Float]
                if (output_exe.corresponds(output_int)(_ == _)) {
                  writeln(w, "results eq-by-user")
                  println("results eq-by-user")
                }
                else {
                  writeln(w, "results ne-by-user")
                  println("results ne-by-user")
                }
              }
              else {
                println("pass-by-user")
                writeln(w, "pass-by-user")
              }
            }
            catch {
              case e: Throwable =>
                println("catch a exception in execator-by-user")
                e.printStackTrace()
                writeln(w, "catch a exception in execator-by-user")
                e.printStackTrace(w)
                return
            }
          case ArrayType(ArrayType(ArrayType(Float, d1), d2), d3) =>
            try {
              val (output_exe: Array[Float], runtime) = Execute(LocalSize, GlobalSize)(code, lowLevel, Args: _*)
              if (RunInterpreter) {
                val output_int = Interpreter(oril).->[Vector[Vector[Vector[Float]]]].runAndFlatten(Args: _*).toArray[Float]
                if (output_exe.corresponds(output_int)(_ == _)) {
                  writeln(w, "results eq-by-user")
                  println("results eq-by-user")
                }
                else {
                  writeln(w, "results ne-by-user")
                  println("results ne-by-user")
                }
              }
              else {
                println("pass-by-user")
                writeln(w, "pass-by-user")
              }
            }
            catch {
              case e: Throwable =>
                println("catch a exception in execator-by-user")
                e.printStackTrace()
                writeln(w, "catch a exception in execator-by-user")
                e.printStackTrace(w)
                return
            }
          case _ =>
            println("Type unimplemented,Ignored-by-user")
            writeln(w, "Type unimplemented,Ignored-by-user")
        }
        */
}

  def tryPrograms(w:PrintWriter):Unit = {
    generateProgram()
    val res = RefinedResult
    for(i <- 0 until res.length){
      val lStr = rewriting.utils.Utils.dumpLambdaToString(res(i))
      println(lStr)
      writeln(w,lStr)
      //val UserFunIndex = lStr.indexOf('\n')

      //The compiler have a bug here, so we need to convert it to string and convert back
      //val l:Lambda = Eval(lStr)
      val l:Lambda = try{
        Eval(lStr)
      }
      catch{
        case e:Throwable=>
          println("parse back failed")
          return
      }

      //prints the basic informations about l
      println("Lambda Num:" + i)
      println(l.toString)
      l.params.foreach(p => println(p.t.toString))


      writeln(w,"Lambda Num:" + i)
      writeln(w,l.toString)
      l.params.foreach(p => writeln(w,p.t.toString))


      trySingleLambda(l,res(i),w)
      w.flush()
      System.out.flush()
    }
  }

  //genrators
  def generateProgram(): Array[Lambda] = {
    //initial input..
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    //ParamList += Param(ArrayType(Float,32))
    //ParamList += Param(ArrayType(Float,32))
    ParamList += Param(Float)
    ParamList += Param(Float)


    val totalRounds = LoopNum
    for(i<- 0 until totalRounds){
      generateLambda()
    }
    refineResult()
    RefinedResult.toArray[Lambda]
  }

  private def generateLambda(): Unit ={
    val totChoiceNum = 7


    val randChoice = util.Random.nextInt(totChoiceNum)
    //randChoice match{
    AssignedChoiceNum match{
      //Join
      case 0 =>
        if(GenJoin) {
          unpackParams()
          generateJoin(LimitNum)
        }
        AssignedChoiceNum += 1
      case 1 =>
        if(GenSplit) {
          unpackParams()
          generateSplit(SplitChunkSize, LimitNum)
        }
        AssignedChoiceNum += 1
      case 2 =>
        if(GenUserFun) {
          unpackParams()
          //generateUserFun(LimitNum)
          generateUserFun(30)
        }
        AssignedChoiceNum += 1
      case 3 =>
        if(GenZip) {
          unpackParams()
          generateZip(LimitNum, ZipLimit)
        }
        AssignedChoiceNum += 1
      case 4 =>
        if(GenGet) {
          unpackParams()
          generateGet(LimitNum)
        }
        AssignedChoiceNum += 1
      case 5 =>
        if(GenMap) {
          unpackParams()
          generateMap(LimitNum)
        }
        AssignedChoiceNum += 1
      case 6 =>
        if(GenReduce) {
          unpackParams()
          generateReduce(LimitNum)
        }
        LimitNum += 10
        AssignedChoiceNum = 0


    }
  }

  private def generateJoin(limitNum:Int): Unit = {
    //val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()


    for(i<- Join_P until ParamList.length){
      ParamList(i).t match{
        case ArrayType(ArrayType(t,m),n) =>
          //pass the type check

          var joinSplit:Boolean = false
          if (ParamToFunCall.contains(ParamList(i))){
            ParamToFunCall(ParamList(i)).f match{
              case sp:Split =>
                joinSplit = true
              case _=>
            }
          }



          //check for Join a split
          /*
          val joinSplit:Boolean = fArg match{
            case fc:FunCall =>
              fc.f match{
                case sp:Split =>
                  true
                case _=>
                  false
              }
            case _=>
              false
          }*/

          if(joinSplit && !AllowJoinASplit){

          }
          else {
            //get the argument of FunCall
            val fArg = getArg(i,PassParamUpPossibility)

            //build the FunCall
            val F = FunCall(new Join(), fArg)

            //set output type
            F.t = ArrayType(t, m * n)

            //build the param corresponds to the FunCall
            val P = Param(F.t)

            //count the parameters of lambda
            val lParams = countParam(F)

            //build the lambda
            val L = Lambda(lParams.toArray[Param], F)

            tempParamList += P
            tempLambdaList += L
            tempParamToFunCall += ((P, F))
          }
        case _=>
      }
    }
    Join_P = ParamList.length

    val resLen = tempParamList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        if(!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    }
    else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }
  }

  private def generateSplit(ChunkSize:Int,limitNum:Int): Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    for(i<- Split_P until ParamList.length){
      ParamList(i).t match{
        case ArrayType(t,n) =>
          if(n.eval >=ChunkSize){
            //Pass the type check!

            //get the argument of FunCall
            val fArg = getArg(i,PassParamUpPossibility)

            //build the FunCall
            val F = FunCall(new Split(ChunkSize),fArg)

            //set output type
            F.t = ArrayType(ArrayType(t,ChunkSize),n /^ ChunkSize)

            //build the param corresponds to the FunCall
            val P = Param(F.t)

            //count the parameters of lambda
            val lParams = countParam(F)

            //build the lambda
            val L = Lambda(lParams.toArray[Param],F)

            tempParamList += P
            tempLambdaList += L
            tempParamToFunCall += ((P,F))

          }
        case _=>
      }
    }
    Split_P = ParamList.length

    val resLen = tempParamList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        if(!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    }
    else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }

  }

  private def generateUserFun(limitNum:Int):Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param, FunCall]()
    val add = UserFun("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float).setScalaFun(xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float])


    for (i1 <- ParamList.indices) {
      for (i2 <- ParamList.indices) {
        if (!Add_Check((i1, i2))) {
          if (ParamList(i1).t == Float && ParamList(i2).t == Float) {
            //check for consequentUserFun
            var containsUserFun = false
            if (ParamToFunCall.contains(ParamList(i1))) {
              ParamToFunCall(ParamList(i1)).f match {
                case u: UserFun =>
                  containsUserFun = true
                case _ =>
              }
            }
            if (ParamToFunCall.contains(ParamList(i2))) {
              ParamToFunCall(ParamList(i2)).f match {
                case u: UserFun =>
                  containsUserFun = true
                case _ =>
              }
            }
            /*
              arg1 match{
                case fc:FunCall =>
                  fc.f match{
                    case u:UserFun =>
                      containsUserFun = true
                    case _=>
                  }
                case _=>
              }
              arg2 match{
                case fc:FunCall =>
                  fc.f match{
                    case u:UserFun =>
                      containsUserFun = true
                    case _=>
                  }
                case _=>
              }
              */




            if (containsUserFun && !consequentUserFun) {
              //Don't allow for UserFun(UserFun(...))
            }
            else {
              val arg1 = getArg(i1,PassParamUpPossibility)
              val arg2 = getArg(i2,PassParamUpPossibility)

              val F = FunCall(add, arg1, arg2)
              F.t = Float
              val P = Param(F.t)
              //count the parameters of lambda
              val lParams = countParam(F)

              //build the lambda
              val L = Lambda(lParams.toArray[Param], F)

              tempParamList += P
              tempLambdaList += L
              tempParamToFunCall += ((P, F))
            }
          }
          Add_Check += ((i1, i2))
          Add_Check += ((i2, i1))
        }
      }
    }

    val resLen = tempParamList.length
    if (resLen > limitNum) {
      for (i <- 0 until limitNum) {
        val randRes = util.Random.nextInt(resLen)
        if (!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    }
    else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }
  }

  private def generateReduce(limitNum:Int): Unit ={

    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param, FunCall]()

    if(ReduceStrictMatchUnpack) {
      //1. Search for proper Lambda

      for (oriLambdaIndex <- LambdaList.indices) {

        //1. Have at least 2 params
        if (LambdaList(oriLambdaIndex).params.length >= 2 && (!Reduce_Lambda_Check(oriLambdaIndex))) {

          val oriLambda = LambdaList(oriLambdaIndex)

          //2. Exist a Init Init.t == L.t
          for (initParamIndexOfLambda <- oriLambda.params.indices) {
            val TofInit = oriLambda.params(initParamIndexOfLambda).t
            if (oriLambda.body.t == TofInit) {

              //3. choose a Ele, ele comes from unpack and ele != Init
              for(eleParamIndexOfLambda <- oriLambda.params.indices){
                if((eleParamIndexOfLambda != initParamIndexOfLambda) && UnpackedToExpr.contains(oriLambda.params(eleParamIndexOfLambda))){

                  //4. choose argInit, argInit.t == Init.t
                  for (argInitIndex <- ParamList.indices) {
                    if (ParamList(argInitIndex).t == TofInit) {

                      //5. argEle comes from unpack
                      val argEle = UnpackedToExpr(oriLambda.params(eleParamIndexOfLambda))


                      //Don't do reductions on array with length 1
                      //The opencl generator causes bugs here
                      if(argEle.t.asInstanceOf[ArrayType].len.eval > 1){
                        //Pass the type check!!!!!!!!!!!!!!!

                        //create new lambda for reduction(base on the original one)
                        //only use two param for this lambda ,deal with other params outside
                        val L2 = replaceParam(
                          replaceParam(Lambda(Array(LambdaList(oriLambdaIndex).params(initParamIndexOfLambda), LambdaList(oriLambdaIndex).params(eleParamIndexOfLambda)), LambdaList(oriLambdaIndex).body),
                            LambdaList(oriLambdaIndex).params(initParamIndexOfLambda), Param(LambdaList(oriLambdaIndex).params(initParamIndexOfLambda).t)),
                          LambdaList(oriLambdaIndex).params(eleParamIndexOfLambda), Param(LambdaList(oriLambdaIndex).params(eleParamIndexOfLambda).t))

                        //generate args

                        val argInit: Expr = ParamList(argInitIndex) match {
                          case p if ParamToFunCall.contains(p) =>
                            //TODO: allow for passing up
                            ParamToFunCall(p)
                          case _ =>
                            //to avoid use init value multiple times:
                            Param(ParamList(argInitIndex).t)
                        }

                        val F = TofInit match{
                          case t1 if t1 == oriLambda.params(eleParamIndexOfLambda).t => FunCall(Reduce(L2), argInit, argEle)
                          case _ => FunCall(ReduceSeq(L2), argInit, argEle)
                        }
                        F.t = ArrayType(TofInit, 1)
                        val P = Param(F.t)
                        val Args = countParam(F)
                        val L3 = Lambda(Args.toArray[Param], F)

                        //TypeChecker(L3)

                        tempLambdaList += L3
                        tempParamList += P
                        tempParamToFunCall += ((P, F))

                      }
                    }
                  }
                }
              }
            }
          }
        }
        Reduce_Lambda_Check += oriLambdaIndex
      }
    }
    else {

      //1. Search for proper Lambda

      for (oriLambdaIndex <- LambdaList.indices) {

        //1. Have at least 2 params
        if (LambdaList(oriLambdaIndex).params.length >= 2 && (!Reduce_Lambda_Check(oriLambdaIndex))) {
          //flag satisfied means: this lambda "could be" a lambda for a reduction
          var satisfied = false

          //2. Exist a Init Init.t == L.t
          for (initParamIndexOfLambda <- LambdaList(oriLambdaIndex).params.indices) {
            val TofInit = LambdaList(oriLambdaIndex).params(initParamIndexOfLambda).t
            if (LambdaList(oriLambdaIndex).body.t == TofInit) {
              satisfied = true

              //3. choose a Ele, Ele != Init
              //Note: Here I use random-choice..
              //It seems strange but I just want to avoid meaningless ones
              //Maybe enumerations is better,who knows?
              //for(k <- LambdaList(i).params.indices){
              //if(k!= j){
              var eleParamIndexOfLambda = -1
              do {
                eleParamIndexOfLambda = util.Random.nextInt(LambdaList(oriLambdaIndex).params.length)
              } while (eleParamIndexOfLambda == initParamIndexOfLambda)
              val TofEle = LambdaList(oriLambdaIndex).params(eleParamIndexOfLambda).t

              //4. choose argInit, argInit.t == Init.t
              for (argInitIndex <- ParamList.indices) {
                if (ParamList(argInitIndex).t == TofInit) {

                  //5. choose argEle, argEle.t == ArrayType(EleT)
                  for (argEleIndex <- ParamList.indices) {
                    ParamList(argEleIndex).t match {
                      case ArrayType(TofEle, eleLength) =>

                        //Don't do reductions on array with length 1
                        //The opencl generator causes bugs here
                        if (eleLength.eval > 1) {

                          //Pass the type check!!!!!!!!!!!!!!!
                          if (!Reduce_L_PI_PE((oriLambdaIndex, argInitIndex, argEleIndex))) {


                            //create new lambda for reduction(base one the original one)
                            //only use two param for this lambda ,deal with other params outside
                            val L2 = replaceParam(
                              replaceParam(Lambda(Array(LambdaList(oriLambdaIndex).params(initParamIndexOfLambda), LambdaList(oriLambdaIndex).params(eleParamIndexOfLambda)), LambdaList(oriLambdaIndex).body),
                                LambdaList(oriLambdaIndex).params(initParamIndexOfLambda), Param(LambdaList(oriLambdaIndex).params(initParamIndexOfLambda).t)),
                              LambdaList(oriLambdaIndex).params(eleParamIndexOfLambda), Param(LambdaList(oriLambdaIndex).params(eleParamIndexOfLambda).t))

                            //generate args

                            val argInit: Expr = ParamList(argInitIndex) match {
                              case p if ParamToFunCall.contains(p) =>
                                //TODO: allow for passing up
                                ParamToFunCall(p)
                              case _ =>
                                //to avoid use init value multiple times:
                                Param(ParamList(argInitIndex).t)
                            }
                            val argEle: Expr = getArg(argEleIndex, PassParamUpPossibility)

                            //to avoid use init value multiple times:
                            //val F = FunCall(Reduce(L2), ParamList(j1), ParamList(k1))
                            val F = FunCall(Reduce(L2), argInit, argEle)
                            F.t = ArrayType(TofInit, 1)
                            val P = Param(F.t)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)

                            //TypeChecker(L3)

                            tempLambdaList += L3
                            tempParamList += P
                            tempParamToFunCall += ((P, F))

                            Reduce_L_PI_PE += ((oriLambdaIndex, argInitIndex, argEleIndex))
                          }
                        }
                      case _ =>
                    }
                  }
                }
              }
            }
          }
          if (!satisfied) {
            Reduce_Lambda_Check += oriLambdaIndex
          }
        }
      }
    }
    val resLen = tempParamList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        if(!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    }
    else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }
  }

  private def generateMap(limitNum:Int):Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param, FunCall]()

    if (MapStrictMatchUnpack) {
      //1. Search for proper lambda
      for (i <- Map_Checked until LambdaList.length) {

        //the map must contains a userfun nested deep inside
        val oriLambda = LambdaList(i)

        if (oriLambda.toString.contains("add")) {


          //2. choose one as the param. The param must comes from unpack
          for(paramIndexOfLambda <- oriLambda.params.indices){
            if(UnpackedToExpr.contains(oriLambda.params.indices(paramIndexOfLambda))){
              val TofParam = oriLambda.params(paramIndexOfLambda).t
              val argEle = UnpackedToExpr(oriLambda.params(paramIndexOfLambda))

              //create new lambda for map(base one the original one)
              //only use one param for this lambda ,deal with other params outside the map
              val L2 = replaceParam(Lambda(Array[Param](oriLambda.params(paramIndexOfLambda)), oriLambda.body)
                , oriLambda.params(paramIndexOfLambda), Param(TofParam))

              //build the funcall
              val F = FunCall(ir.ast.Map(L2), argEle)
              F.t = ArrayType(LambdaList(i).body.t, argEle.t.asInstanceOf[ArrayType].len)

              //build the param corresponds to the funcall
              val P = Param(F.t)

              //count the params
              val lParam = countParam(F)
              val L3 = Lambda(lParam.toArray[Param], F)

              //TypeChecker(L3)


              tempLambdaList += L3
              tempParamList += P
              tempParamToFunCall += ((P, F))
            }
          }
        }
      }
      Map_Checked = LambdaList.length

    }
    else {

      //1. Search for proper lambda
      for (i <- LambdaList.indices) {

        //the map must contains a userfun nested deep inside
        val oriLambda = LambdaList(i)

        if (oriLambda.toString.contains("add")) {


          //2. choose one as the param
          val paramIndexOfLambda = util.Random.nextInt(oriLambda.params.length)

          //for (j <- LambdaList(i).params.indices){
          //Get the type of it
          val TofParam = oriLambda.params(paramIndexOfLambda).t

          //3. search for a proper Arg.t == ArrayType(TofParam)
          for (argIndex <- ParamList.indices) {
            ParamList(argIndex).t match {
              case ArrayType(TofParam, eleLength) =>

                //Pass the Type check!
                if (!Map_L_E((i, argIndex))) {

                  //create new lambda for map(base one the original one)
                  //only use one param for this lambda ,deal with other params outside the map
                  val L2 = replaceParam(Lambda(Array[Param](oriLambda.params(paramIndexOfLambda)), oriLambda.body)
                    , oriLambda.params(paramIndexOfLambda), Param(TofParam))

                  //generate args
                  val argEle = getArg(argIndex, PassParamUpPossibility)

                  //build the funcall
                  val F = FunCall(ir.ast.Map(L2), argEle)
                  F.t = ArrayType(LambdaList(i).body.t, eleLength)

                  //build the param corresponds to the funcall
                  val P = Param(F.t)

                  //count the params
                  val lParam = countParam(F)
                  val L3 = Lambda(lParam.toArray[Param], F)

                  //TypeChecker(L3)


                  tempLambdaList += L3
                  tempParamList += P
                  tempParamToFunCall += ((P, F))
                  Map_L_E += ((i, argIndex))
                }

              case _ =>
            }
          }
        }
      }
    }

    val resLen = tempParamList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        if(!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    }
    else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }
  }

  private def generateZip(limitNum:Int,zipArrayLimit:Int):Unit ={
    assert(zipArrayLimit >= 2)
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()


    for(i <- Zip_P until ParamList.length){
      ParamList(i).t match{
        //1. A0 should have an arrayType
        case ArrayType(a0T,a0Len) =>

          //2. AId : id of params that have the same type with A0
          val AId = scala.collection.mutable.ArrayBuffer[Int](i)
          for(j <- 0 until ParamList.length){
            ParamList(j).t match{
              case ArrayType(_,`a0Len`) =>
                AId += j
              case _=>
            }
          }

          //3. should have at least 2 elements
          if(AId.length >= 2){
            //Pass the type check!

            //randomly choose 'argNum' of params from AId
            val argNum = AId.length match{
              case temp1 if temp1 < zipArrayLimit =>
                util.Random.nextInt(temp1 - 1) + 2
              case _ =>
                util.Random.nextInt(zipArrayLimit - 1) + 2
            }

            //get the argument of f
            val Args = scala.collection.mutable.ArrayBuffer[Expr](getArg(AId(0),PassParamUpPossibility))
            for(i <- 0 until argNum - 1){
              Args += getArg(AId(util.Random.nextInt(AId.length)),PassParamUpPossibility)
            }

            //build the funcall
            val F = FunCall(Zip(argNum),Args:_*)

            //set the output type
            F.t = TypeChecker(F)

            //build the param corresponds to the FunCall
            val P = Param(F.t)

            //count the parameters of lambda
            val lParams = countParam(F)

            //build the lambda
            val L = Lambda(lParams.toArray[Param], F)

            tempParamList += P
            tempLambdaList += L
            tempParamToFunCall += ((P, F))
          }

        case _=>

      }
    }
    Zip_P = ParamList.length

    val resLen = tempParamList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        if(!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    }
    else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }

  }

  private def generateGet(limitNum:Int):Unit ={
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()


    for(i <- Get_P until ParamList.length){
      ParamList(i).t match{
        case tt: TupleType =>

          //Pass the type check!

          //build the funcall
          val F = FunCall(Get(util.Random.nextInt(tt.elemsT.length)),getArg(i,PassParamUpPossibility))

          //set the output type
          F.t = TypeChecker(F)

          //build the param corresponds to the FunCall
          val P = Param(F.t)

          //count the parameters of lambda
          val lParams = countParam(F)

          //build the lambda
          val L = Lambda(lParams.toArray[Param], F)

          tempParamList += P
          tempLambdaList += L
          tempParamToFunCall += ((P, F))

        case _=>
      }
    }

    Get_P = ParamList.length

    val resLen = tempParamList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        if(!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    }
    else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }

  }

  private def unpackParams():Unit={
    if(ReduceStrictMatchUnpack || MapStrictMatchUnpack){
      val tempParamList = ArrayBuffer[Param]()
      for(i <- UnPack_P until ParamList.length){
        ParamList(i).t match{
          case ArrayType(t, n) =>
            val tempParam = Param(t)
            tempParamList += tempParam
            UnpackedToExpr += ((tempParam -> getArg(i,0)))
          case _ =>
        }
      }
      ParamList ++= tempParamList
      UnPack_P = ParamList.length
    }
    else {
      val tempParamList = ArrayBuffer[Param]()
      for (i <- UnPack_P until ParamList.length) {
        ParamList(i).t match {
          case ArrayType(t, n) =>
            tempParamList += Param(t)
          case _ =>
        }
      }
      ParamList ++= tempParamList
      UnPack_P = ParamList.length
    }
  }
  //helper functions

  private def refineParamToFunCall(oriLambda:Lambda):Lambda={
    val refineParamList = ArrayBuffer[Param]()
    for(i <- oriLambda.params.indices){
      if(ParamToFunCall.contains(oriLambda.params(i))){
        refineParamList += oriLambda.params(i)
      }
    }
    if(refineParamList.nonEmpty){
      var L2 = Lambda(refineParamList.toArray[Param],oriLambda.body)


      //replace them with new param
      for (j <- refineParamList.indices) {
        L2 = replaceParam(L2,refineParamList(j),Param(refineParamList(j).t))
      }

      //create a funcall for it
      val F = FunCall(L2,refineParamList.map(
        ParamToFunCall(_)
      ).toArray[Expr]:_*)

      val lParam = countParam(F)
      val L = Lambda(lParam.toArray[Param],F)

      refineParamToFunCall(L)
    }
    else{
      oriLambda
    }
  }
  private def refineUnpack(oriLambda:Lambda):Lambda={
    for(i <- oriLambda.params.indices){
      if(UnpackedToExpr.contains(oriLambda.params(i))){

        var L2 = Lambda(Array[Param](oriLambda.params(i)),oriLambda.body)

        L2 = replaceParam(L2,oriLambda.params(i),Param(oriLambda.params(i).t))

        val argEle = UnpackedToExpr(oriLambda.params(i))
        val F = FunCall(ir.ast.Map(L2), argEle)

        val lParam = countParam(F)
        val L3 = Lambda(lParam.toArray[Param], F)
        return refineUnpack(L3)
      }
    }
    oriLambda
  }
  private def refineOneLambda(oriLambda:Lambda):Lambda={
    for(i <- oriLambda.params.indices){
      if(UnpackedToExpr.contains(oriLambda.params(i)) || ParamToFunCall.contains(oriLambda.params(i))){
        return refineOneLambda(refineUnpack(refineParamToFunCall(oriLambda)))
      }
    }
    oriLambda
  }
  private def refineResult():Unit={
    for(i<- LambdaList.indices){
      val afterRefine = refineOneLambda(LambdaList(i))
      if((MustContainsMap && !afterRefine.toString.contains("Map")) ||
        MustContainsUserFun && !afterRefine.toString.contains("add")){

      }
      else{
        RefinedResult += afterRefine
      }
    }
  }
  /*
  private def refineResult():Unit={
    for(i <- LambdaList.indices){
      val oriLambda = LambdaList(i)
      if((MustContainsMap && !oriLambda.toString.contains("Map")) ||
        MustContainsUserFun && !oriLambda.toString.contains("add")){

      }
      else{
        //the params in refine param list will be replace as funcalls
        val refineParamList = ArrayBuffer[Param]()
        for(j <- oriLambda.params.indices){
          if(ParamToFunCall.contains(oriLambda.params(j))){
            refineParamList += oriLambda.params(j)
          }
        }
        if(refineParamList.nonEmpty) {
          //create a lambda without unknown params
          var L2 = Lambda(refineParamList.toArray[Param],oriLambda.body)


          //replace them with new param
          for (j <- refineParamList.indices) {
             L2 = replaceParam(L2,refineParamList(j),Param(refineParamList(j).t))
          }

          //create a funcall for it
          val F = FunCall(L2,refineParamList.map(
            ParamToFunCall(_)
          ).toArray[Expr]:_*)

          val lParam = countParam(F)

          assert(lParam.forall(!ParamToFunCall.contains(_)))
          /*
          for(i <- lParam.indices){
            if(ParamToFunCall.contains(lParam(i))){
              val test1 = oriLambda
              val test2 = refineParamList
              val test3 = ParamList
              val test4 = L2

            }
          }*/

          val L = Lambda(lParam.toArray[Param],F)

          RefinedResult += L

        }
        else{
          RefinedResult += oriLambda
        }
      }
    }
  }*/
  private def getArg(id:Int,possibility:Double):Expr ={
    if(ParamToFunCall.contains(ParamList(id))) {
      val randF = scala.util.Random.nextFloat()
      if (randF < possibility) {
        //pass the param up, return the param
        ParamList(id)
      }
      else {
        //calculate the param here, return the corresponding
        ParamToFunCall(ParamList(id))
      }
    }
    else{
      ParamList(id)
    }
  }
  private def writeln(w:PrintWriter,s:String):Unit={
    w.write(s+"\n")
  }
  private def replaceParam(p:Param,oldP:Param,newP:Param):Param ={
    if(p.eq(oldP)){
      newP
    }
    else{
      p
    }
  }
  private def replaceParam(l: Lambda, oldP: Param, newP: Param) : Lambda = {
    val newBody = replaceParam(l.body, oldP, newP)
    val newParams = l.params.map((p) => replaceParam(p,oldP,newP))
    if (!newBody.eq(l.body) || (newParams, l.params).zipped.exists( (e1, e2) => !e1.eq(e2)) )
      Lambda(newParams,newBody)
    else
      l
  }
  private def replaceParam(e: Expr, oldP: Param, newP: Param): Expr = {
    if (e.eq(oldP)) {
      newP
    } else {
      e match {
        case call: FunCall =>
          val newArgs = call.args.map((arg) => replaceParam(arg, oldP, newP))

          val newCall = call.f match {

            case fp: FPattern =>
              // Try to do the replacement in the body
              val replaced = replaceParam(fp.f, oldP, newP)

              // If replacement didn't occur return fp
              // else instantiate a new pattern with the updated lambda
              if (fp.f.eq(replaced))
                fp
              else
                fp.copy(replaced)

            case l: Lambda =>
              // Try to do the replacement in the body
              val replaced = replaceParam(l, oldP, newP)

              // If replacement didn't occur return l
              // else instantiate the updated lambda
              if (l.eq(replaced))
                l
              else
                replaced

            case other => other
          }

          if (!newCall.eq(call.f) || (newArgs, call.args).zipped.exists( (e1, e2) => !e1.eq(e2)) ) {
            // Instantiate a new FunCall if anything has changed
            FunCall(newCall, newArgs: _*)
          } else
            e // Otherwise return the same FunCall object

        case _ => e
      }
    }
  }
  private def countParam(L:Lambda):ArrayBuffer[Param]={
    (countParam(L.body) -- L.params.toBuffer[Param]).distinct
  }
  private def countParam(p:Pattern):ArrayBuffer[Param]={
    p match{
      case red: ir.ast.Reduce =>
        countParam(red.f)
      case m: ir.ast.Map =>
        countParam(m.f)
      case _=>
        ArrayBuffer[Param]()
    }
  }
  private def countParam(Fc:FunCall):ArrayBuffer[Param] ={
    val rs = ArrayBuffer[Param]()
    Fc.f match{
      case l:Lambda =>
        rs ++= countParam(l)
      case p:Pattern =>
        rs ++= countParam(p)
      case _=>
    }
    for(i<- Fc.args.indices){
      rs ++= countParam(Fc.args(i))
    }
    rs.distinct
  }
  private def countParam(E:Expr):ArrayBuffer[Param] ={
    E match{
      case fc:FunCall =>
        countParam(fc)
      case p:Param =>
        ArrayBuffer[Param](p)
    }
  }
}