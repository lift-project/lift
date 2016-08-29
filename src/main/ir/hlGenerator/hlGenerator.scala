package ir.hlGenerator

import java.io.{PrintWriter, Writer}

import collection.mutable._
import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Eval, Execute}
import opencl.ir._
import opencl.ir.pattern.{ReduceSeq, toGlobal}
import rewriting.Rules._
import rewriting.{EnabledMappings, Lower, Rewrite, Rules}

import scala.language.reflectiveCalls
/*
object hlGenerator{
  var ParamList: ArrayBuffer[Param] = new ArrayBuffer[Param]()
  var FunCallList: ArrayBuffer[FunCall] = new ArrayBuffer[FunCall]()
  var LambdaList: ArrayBuffer[Lambda] = new ArrayBuffer[Lambda]()



  //Used for debug
  val LoopNum = 35
  val consequentUserFun = false
  val ReduceInitToGlobal = false
  var RunInterpreter = false
  var AssignedChoiceNum = 0
  //avoid for redundant
  //Join
  var Join_P = 0
  var Join_F = 0
  //Split
  var Split_P = 0
  var Split_F = 0
  //Reduce
  var Reduce_L_I_E = scala.collection.mutable.Set[(Int,(Int,Int),(Int,Int))]()
  var Reduce_Lambda_Check = scala.collection.mutable.Set[Int]()
  //Map
  var Map_L_E = scala.collection.mutable.Set[(Int,(Int,Int))]()
  //Zip
  var Zip_P = 0
  var Zip_F = 0
  //Get
  var Get_P = 0
  var Get_F = 0

  //UserFun
  var Add_Check = scala.collection.mutable.Set[((Int,Int),(Int,Int))]()
  var Add_Check_FunCall = scala.collection.mutable.Set[Int]()

  def resetAll():Unit={
    ParamList = new ArrayBuffer[Param]()
    FunCallList = new ArrayBuffer[FunCall]()
    LambdaList = new ArrayBuffer[Lambda]()

    //avoid for redundant
    //Join
    Join_P = 0
    Join_F = 0
    //Split
    Split_P = 0
    Split_F = 0
    //Reduce
    Reduce_L_I_E = scala.collection.mutable.Set[(Int,(Int,Int),(Int,Int))]()
    Reduce_Lambda_Check = scala.collection.mutable.Set[Int]()
    //Map
    Map_L_E = scala.collection.mutable.Set[(Int,(Int,Int))]()
    //Zip
    Zip_P = 0
    Zip_F = 0
    //Get
    Get_P = 0
    Get_F = 0

    //UserFun
    Add_Check = scala.collection.mutable.Set[((Int,Int),(Int,Int))]()
    Add_Check_FunCall = scala.collection.mutable.Set[Int]()
  }

  private def writeln(w:PrintWriter,s:String):Unit={
    w.write(s+"\n")
  }

  def trySingleLambda(l:Lambda,oril:Lambda,w:PrintWriter):Unit = {
    //Requires an add in it
    if (l.toString.contains("add") && l.toString.contains("Map")) {
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
          case _=>
        }
      }
      //2. Pass the TypeChecker
      val outType = TypeChecker(l)

      //3. lower the lambda
      var fs = List[Lambda]()
      try {
        fs = Lower.mapCombinations(l, new EnabledMappings(true, false, false, false, false, false))
      }
      catch{
        case e:Throwable =>
          println("catch a exception in lower-parser-by-user")
          e.printStackTrace()
          writeln(w,"catch a exception in lower-parser-by-user")
          e.printStackTrace(w)
          return
      }

      val lowLevel = fs.head

      //4. compile the lambda
      var code = ""
      try {
        println(lowLevel.toString)
        writeln(w,lowLevel.toString)
        code = Compile(lowLevel)
      }
      catch{
        case e:Throwable =>
          println("catch a exception in compiler-by-user")
          writeln(w,"catch a exception in compiler-by-user")
          e.printStackTrace(w)
          return
      }
      //5. execute the OpenCL kernel and the interpreter
      outType match{
        case Float =>
          try {
            val(output_exe:Float,runtime) = Execute(1,1)(code,lowLevel,Args:_*)
            if(RunInterpreter) {

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
            else{
              println("pass-by-user")
              writeln(w,"pass-by-user")
            }

          }
          catch{
            case e:Throwable =>
              println("catch a exception in execator-by-user")
              e.printStackTrace()
              writeln(w,"catch a exception in execator-by-user")
              e.printStackTrace(w)
              return
          }
        case ArrayType(Float,d1) =>
          try {
            val(output_exe:Array[Float],runtime)= Execute(1,d1.eval)(code,lowLevel,Args:_*)
            if(RunInterpreter) {
              val output_int = Interpreter(oril).->[Vector[Float]].run(Args: _*).toArray[Float]
              if (output_exe.corresponds(output_int)(_ == _)) {
                writeln(w, "results eq-by-user")
              }
              else {
                writeln(w, "results ne-by-user")
              }
            }
            else{
              println("pass-by-user")
              writeln(w,"pass-by-user")
            }
          }
          catch{
            case e:Throwable =>
              println("catch a exception in execator-by-user")
              e.printStackTrace()
              writeln(w,"catch a exception in execator-by-user")
              e.printStackTrace(w)
              return
          }
        case ArrayType(ArrayType(Float,d1),d2)=>
          try {
            val(output_exe:Array[Float],runtime)= Execute(1,d1.eval*d2.eval)(code,lowLevel,Args:_*)
            if(RunInterpreter) {
              val output_int = Interpreter(oril).->[Vector[Vector[Float]]].runAndFlatten(Args: _*).toArray[Float]
              if (output_exe.corresponds(output_int)(_ == _)) {
                writeln(w, "results eq-by-user")
              }
              else {
                writeln(w, "results ne-by-user")
              }
            }
            else{
              println("pass-by-user")
              writeln(w,"pass-by-user")
            }
          }
          catch{
            case e:Throwable =>
              println("catch a exception in execator-by-user")
              e.printStackTrace()
              writeln(w,"catch a exception in execator-by-user")
              e.printStackTrace(w)
              return
          }
        case ArrayType(ArrayType(ArrayType(Float,d1),d2),d3)=>
          try {
            val(output_exe:Array[Float],runtime) = Execute(1,d1.eval*d2.eval*d3.eval)(code,lowLevel,Args:_*)
            if(RunInterpreter) {
              val output_int = Interpreter(oril).->[Vector[Vector[Vector[Float]]]].runAndFlatten(Args: _*).toArray[Float]
              if (output_exe.corresponds(output_int)(_ == _)) {
                writeln(w, "results eq-by-user")
              }
              else {
                writeln(w, "results ne-by-user")
              }
            }
            else{
              println("pass-by-user")
              writeln(w,"pass-by-user")
            }
          }
          catch{
            case e:Throwable =>
              println("catch a exception in execator-by-user")
              e.printStackTrace()
              writeln(w,"catch a exception in execator-by-user")
              e.printStackTrace(w)
              return
          }
        case _=>
          println("Type unimplemented,Ignored-by-user")
          writeln(w,"Type unimplemented,Ignored-by-user")
      }



    }
    else{
      println("Doesn't contains UserFun or Map, Ignored-by-user")
      writeln(w,"Doesn't contains UserFun or Map, Ignored-by-user")
      return
    }
  }

  def tryPrograms(w:PrintWriter):Unit = {
    generateProgram()
    val res = LambdaList
    for(i <- 0 until res.length){
      val lStr = rewriting.utils.Utils.dumpLambdaToString(res(i))
      println(lStr)
      //val UserFunIndex = lStr.indexOf('\n')

      val l:Lambda = Eval(lStr)
      //val l = res(i)
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
      //("rm ~/TempLambdas/tempLambda.txt").!
    }
  }

  def generateProgram(): Unit = {
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    ParamList += Param(ArrayType(Float,32))
    ParamList += Param(ArrayType(Float,32))
    ParamList += Param(Float)
    ParamList += Param(Float)
    val totalRounds = LoopNum
    for(i<- 0 until totalRounds){
      generateLambda()
      val test = LambdaList
      val test1 = FunCallList
    }
  }




  private def getArg(id:Int):Expr = {
    val ParamLength = ParamList.length
    val FunCallLength = FunCallList.length
    id match{
      case t if t<ParamLength =>
        ParamList(t)
      case t if t >= ParamLength =>
        FunCallList(t-ParamLength)
    }
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
  private def generateLambda(): Unit ={
    val totChoiceNum = 7
    val SplitChunkSize = 4
    val ZipLimit = 4
    val randChoice = util.Random.nextInt(totChoiceNum)
    //randChoice match{
    AssignedChoiceNum match{
      //Join
      case 0 =>
        matchJoin(30)
        AssignedChoiceNum += 1
      case 1 =>
        matchSplit(SplitChunkSize,30)
        AssignedChoiceNum += 1
      case 2 =>
        matchUserFun(30)
        AssignedChoiceNum += 1
      case 3 =>
        //matchZip(30,ZipLimit)
        AssignedChoiceNum += 1
      case 4 =>
        //matchGet(30)
        AssignedChoiceNum += 1
      case 5 =>
        matchMap(30)
        AssignedChoiceNum += 1
      case 6 =>
        matchReduce(30)
        AssignedChoiceNum = 0


    }
  }
  private def matchReduce(limitNum:Int): Unit ={
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    //1. Search for proper Lambda
    //val LambdaLength = LambdaList.length
    for(i <- LambdaList.indices){
      //1. Have at least 2 params
      if(LambdaList(i).params.length >= 2 && (!Reduce_Lambda_Check(i))){
        //2. Exist a Init -> j, Init.t == L.t
        var satisfied = false
        for(j <- LambdaList(i).params.indices){
          val TofJ = LambdaList(i).params(j).t
          if(LambdaList(i).body.t == TofJ){
            satisfied = true
            var k = -1
            do{
              k = util.Random.nextInt(LambdaList(i).params.length)
            }while(k==j)
            //3. choose a Ele -> k, k!= j
            //Here we use random-choice.. maybe enumerations is better,who knows?
            //for(k <- LambdaList(i).params.indices){
            //if(k!= j){
            val TofK = LambdaList(i).params(k).t
            //4. choose arg1, arg1.t == init.t => j1
            for(j1 <- ParamList.indices){
              if(ParamList(j1).t == LambdaList(i).params(j).t){
                //5. choose arg2, arg2.t == ArrayType(Ele) => k1
                for(k1 <- ParamList.indices){
                  ParamList(k1).t match{
                    case ArrayType(TofK,eleLength) =>
                      if(eleLength.eval > 1) {
                        //Pass the type check!!!!!!!!!!!!!!!
                        if (!Reduce_L_I_E((i, (1, j1), (1, k1)))) {
                          val L2 = replaceParam(
                            replaceParam(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                              LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                            LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                          //to avoid use init value multiple times:
                          //val F = FunCall(Reduce(L2), ParamList(j1), ParamList(k1))
                          val F = FunCall(Reduce(L2), Param(ParamList(j1).t), ParamList(k1))
                          F.t = ArrayType(TofJ, 1)
                          val Args = countParam(F)
                          val L3 = Lambda(Args.toArray[Param], F)
                          tempFunCallList += F
                          tempLambdaList += L3
                          Reduce_L_I_E += ((i, (1, j1), (1, k1)))
                        }
                      }
                    case _=>
                  }
                }
                for(k1 <- FunCallList.indices){
                  FunCallList(k1).t match {
                    case ArrayType(TofK, eleLength) =>
                      if (eleLength.eval > 1) {
                        //Pass the type check!!!!!!!!!!!
                        if (!Reduce_L_I_E((i, (1, j1), (2, k1)))) {
                          val L2 = replaceParam(
                            replaceParam(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                              LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                            LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                          //to avoid use init value multiple times:
                          //val F = FunCall(Reduce(L2), ParamList(j1), FunCallList(k1))
                          val F = FunCall(Reduce(L2), Param(ParamList(j1).t), FunCallList(k1))
                          F.t = ArrayType(TofJ, 1)
                          val Args = countParam(F)
                          val L3 = Lambda(Args.toArray[Param], F)
                          tempFunCallList += F
                          tempLambdaList += L3
                          Reduce_L_I_E += ((i, (1, j1), (2, k1)))
                        }
                      }
                    case _=>
                  }
                }
              }
            }
            for(j1 <- FunCallList.indices){
              if(FunCallList(j1).t == LambdaList(i).params(j).t){
                //5. choose arg2, arg2.t == ArrayType(Ele) => k1
                for(k1 <- ParamList.indices){
                  ParamList(k1).t match{
                    case ArrayType(TofK,eleLength) =>
                      if(eleLength.eval > 1) {
                        //Pass the type check!!!!!!!!!!!!!!!
                        if (!Reduce_L_I_E((i, (2, j1), (1, k1)))) {
                          val L2 = replaceParam(
                            replaceParam(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                              LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                            LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                          if(ReduceInitToGlobal){
                            val F = FunCall(Reduce(L2), FunCall(toGlobal(FunCallList(j1).f),FunCallList(j1).args:_*), ParamList(k1))
                            F.t = ArrayType(TofJ, 1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            tempFunCallList += F
                            tempLambdaList += L3
                            Reduce_L_I_E += ((i, (2, j1), (1, k1)))
                          }
                          else {
                            val F = FunCall(Reduce(L2), FunCallList(j1), ParamList(k1))
                            F.t = ArrayType(TofJ, 1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            tempFunCallList += F
                            tempLambdaList += L3
                            Reduce_L_I_E += ((i, (2, j1), (1, k1)))
                          }
                        }
                      }
                    case _=>
                  }
                }
                for(k1 <- FunCallList.indices){
                  FunCallList(k1).t match{
                    case ArrayType(TofK,eleLength) =>
                      if(eleLength.eval>1) {
                        //Pass the type check!!!!!!!!!!!
                        if (!Reduce_L_I_E((i, (2, j1), (2, k1)))) {
                          val L2 = replaceParam(
                            replaceParam(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                              LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                            LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                          if(ReduceInitToGlobal){
                            val F = FunCall(Reduce(L2), FunCall(toGlobal(FunCallList(j1).f),FunCallList(j1).args:_*), FunCallList(k1))
                            F.t = ArrayType(TofJ, 1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            tempFunCallList += F
                            tempLambdaList += L3
                            Reduce_L_I_E += ((i, (2, j1), (2, k1)))
                          }
                          else {
                            val F = FunCall(Reduce(L2), FunCallList(j1), FunCallList(k1))
                            F.t = ArrayType(TofJ, 1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            tempFunCallList += F
                            tempLambdaList += L3
                            Reduce_L_I_E += ((i, (2, j1), (2, k1)))
                          }
                        }
                      }
                    case _=>
                  }
                }
              }
            }
            //}
            //}
          }
        }
        if(!satisfied){
          Reduce_Lambda_Check += i
        }
      }
    }
    val resLen = tempFunCallList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        FunCallList += tempFunCallList(randRes)
        LambdaList += tempLambdaList(randRes)
      }
    }
    else {
      FunCallList ++= tempFunCallList
      LambdaList ++= tempLambdaList
    }
  }
  private def matchJoin(limitNum:Int): Unit = {
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    for(i<- Join_P until ParamList.length){
      ParamList(i).t match{
        case ArrayType(ArrayType(t,m),n) =>
          //Pass the type check!
          val F = FunCall(new Join(),ParamList(i))
          F.t = ArrayType(t,m*n)
          val Args = countParam(F)
          val L = Lambda(Args.toArray[Param],F)
          tempFunCallList += F
          tempLambdaList += L
        case _=>
      }
    }
    Join_P = ParamList.length
    for(i<- Join_F until FunCallList.length){
      FunCallList(i).t match{
        case ArrayType(ArrayType(t,m),n) =>
          //Pass the type check!
          FunCallList(i).f match {
            case s: Split =>
            //Join a Split is meaningless
            case _ =>
              val F = FunCall(new Join(), FunCallList(i))
              F.t = ArrayType(t, m * n)
              val Args = countParam(F)
              val L = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L
          }
        case _=>
      }
    }
    Join_F = FunCallList.length
    val resLen = tempFunCallList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        FunCallList += tempFunCallList(randRes)
        LambdaList += tempLambdaList(randRes)
      }
    }
    else {
      FunCallList ++= tempFunCallList
      LambdaList ++= tempLambdaList
    }
  }
  private def matchSplit(ChunkSize:Int,limitNum:Int): Unit ={
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    for(i<- Split_P until ParamList.length){
      ParamList(i).t match{
        case ArrayType(t,n) =>
          if(n.eval >=ChunkSize){
            //Pass the type check!
            val F = FunCall(new Split(ChunkSize),ParamList(i))
            F.t = ArrayType(ArrayType(t,ChunkSize),n /^ ChunkSize)
            val Args = countParam(F)
            val L = Lambda(Args.toArray[Param],F)
            tempFunCallList += F
            tempLambdaList += L
          }
        case _=>
      }
    }
    Split_P = ParamList.length
    for(i<- Split_F until FunCallList.length){
      FunCallList(i).t match {
        case ArrayType(t, n) =>
          if (n.eval >= ChunkSize) {
            //Pass the type check!
            val F = FunCall(new Split(ChunkSize), FunCallList(i))
            F.t = ArrayType(ArrayType(t, ChunkSize), n /^ ChunkSize)
            val Args = countParam(F)
            val L = Lambda(Args.toArray[Param], F)
            tempFunCallList += F
            tempLambdaList += L
          }
        case _ =>
      }
    }
    Split_F = FunCallList.length
    val resLen = tempFunCallList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        FunCallList += tempFunCallList(randRes)
        LambdaList += tempLambdaList(randRes)
      }
    }
    else {
      FunCallList ++= tempFunCallList
      LambdaList ++= tempLambdaList
    }
  }
  private def matchUserFun(limitNum:Int):Unit ={
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    val add = UserFun("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float).setScalaFun (xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float])
    if(!consequentUserFun){
      for (i1 <- ParamList.indices) {
        for (i2 <- ParamList.indices) {
          if (!Add_Check(((1, i1), (1, i2)))) {
            if (ParamList(i1).t == Float && ParamList(i2).t == Float) {
              val F = FunCall(add, ParamList(i1), ParamList(i2))
              F.t = Float
              val Args = countParam(F)
              val L = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L
            }
            Add_Check += (((1, i1), (1, i2)))
            Add_Check += (((1, i2), (1, i1)))
          }
        }
        for (i2 <- FunCallList.indices) {
          if (!Add_Check(((1, i1), (2, i2)))) {
            if (ParamList(i1).t == Float && FunCallList(i2).t == Float ) {
              FunCallList(i2).f match {
                case u:UserFun =>
                case _=>
                  val F = FunCall (add, ParamList (i1), FunCallList (i2) )
                  F.t = Float
                  val Args = countParam (F)
                  val L = Lambda (Args.toArray[Param], F)
                  tempFunCallList += F
                  tempLambdaList += L
              }
            }
            Add_Check += (((1, i1), (2, i2)))
            Add_Check += (((1, i2), (2, i1)))

          }
        }
      }
      for (i1 <- FunCallList.indices) {
        FunCallList(i1).f match{
          case u:UserFun =>
          case _=>
            for (i2 <- ParamList.indices) {
              if (Add_Check(((2, i1), (1, i2)))) {
                if (FunCallList(i1).t == Float && ParamList(i2).t == Float) {

                      val F = FunCall (add, FunCallList (i1), ParamList (i2) )
                      F.t = Float
                      val Args = countParam (F)
                      val L = Lambda (Args.toArray[Param], F)
                      tempFunCallList += F
                      tempLambdaList += L
                }
                Add_Check += (((2, i1), (1, i2)))
                Add_Check += (((2, i2), (1, i1)))
              }
            }
            for (i2 <- FunCallList.indices) {
              if (!Add_Check(((2, i1), (2, i2)))) {
                if (FunCallList(i1).t == Float && FunCallList(i2).t == Float) {
                  FunCallList(i2).f match {
                    case u:UserFun =>
                    case _=>
                      val F = FunCall (add, FunCallList (i1), FunCallList (i2) )
                      F.t = Float
                      val Args = countParam (F)
                      val L = Lambda (Args.toArray[Param], F)
                      tempFunCallList += F
                      tempLambdaList += L
                  }
                }
                Add_Check += (((2, i1), (2, i2)))
                Add_Check += (((2, i2), (2, i1)))

              }
            }
        }

      }
    }
    else {
      for (i1 <- ParamList.indices) {
        for (i2 <- ParamList.indices) {
          if (!Add_Check(((1, i1), (1, i2)))) {
            if (ParamList(i1).t == Float && ParamList(i2).t == Float) {
              val F = FunCall(add, ParamList(i1), ParamList(i2))
              F.t = Float
              val Args = countParam(F)
              val L = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L
            }
            Add_Check += (((1, i1), (1, i2)))
            Add_Check += (((1, i2), (1, i1)))
          }
        }
        for (i2 <- FunCallList.indices) {
          if (!Add_Check_FunCall(i2) && !Add_Check(((1, i1), (2, i2)))) {
            if (ParamList(i1).t == Float && FunCallList(i2).t == Float) {
              val F = FunCall(add, ParamList(i1), FunCallList(i2))
              F.t = Float
              val Args = countParam(F)
              val L = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L
              Add_Check_FunCall += i2
            }
            Add_Check += (((1, i1), (2, i2)))
            Add_Check += (((1, i2), (2, i1)))

          }
        }
      }
      for (i1 <- FunCallList.indices) {
        for (i2 <- ParamList.indices) {
          if (!Add_Check_FunCall(i1) && !Add_Check(((2, i1), (1, i2)))) {
            if (FunCallList(i1).t == Float && ParamList(i2).t == Float) {
              val F = FunCall(add, FunCallList(i1), ParamList(i2))
              F.t = Float
              val Args = countParam(F)
              val L = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L
              Add_Check_FunCall += i1
            }
            Add_Check += (((2, i1), (1, i2)))
            Add_Check += (((2, i2), (1, i1)))

          }
        }
        for (i2 <- FunCallList.indices) {
          if (!Add_Check_FunCall(i1) && !Add_Check_FunCall(i2) && !Add_Check(((2, i1), (2, i2)))) {
            if (FunCallList(i1).t == Float && FunCallList(i2).t == Float) {
              val F = FunCall(add, FunCallList(i1), FunCallList(i2))
              F.t = Float
              val Args = countParam(F)
              val L = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L
              Add_Check_FunCall += i1
              Add_Check_FunCall += i2
            }
            Add_Check += (((2, i1), (2, i2)))
            Add_Check += (((2, i2), (2, i1)))

          }
        }
      }
    }
    val resLen = tempFunCallList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        FunCallList += tempFunCallList(randRes)
        LambdaList += tempLambdaList(randRes)
      }
    }
    else {
      FunCallList ++= tempFunCallList
      LambdaList ++= tempLambdaList
    }
  }
  private def matchMap(limitNum:Int):Unit ={
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    //1. Search for proper lambda
    for( i<-LambdaList.indices){
      //can use all lambdas
      //2. choose one as the param -> j
      val j = util.Random.nextInt(LambdaList(i).params.length)
      //for (j <- LambdaList(i).params.indices){
      val TofJ = LambdaList(i).params(j).t
      //3. search for a proper Arg.t == ArrayType(j.t)  -> j1
      for( j1 <- ParamList.indices){
        ParamList(j1).t match{
          case ArrayType(TofJ,eleLength) =>
            //if(eleLength.eval > 1) {
            //Pass the Type check!
            if (!Map_L_E((i, (1, j1)))) {
              val L2 = replaceParam(Lambda(Array[Param](LambdaList(i).params(j)), LambdaList(i).body)
                , LambdaList(i).params(j), Param(TofJ))
              val F = FunCall(ir.ast.Map(L2), ParamList(j1))
              F.t = ArrayType(LambdaList(i).body.t, eleLength)
              val Args = countParam(F)
              val L3 = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L3
              Map_L_E += ((i, (1, j1)))
            }
          //}
          case _=>
        }
      }
      for( j1 <- FunCallList.indices){
        FunCallList(j1).t match{
          case ArrayType(TofJ,eleLength) =>
            //if(eleLength.eval>1) {
            //Pass the Type check!
            if (!Map_L_E((i, (2, j1)))) {
              val L2 = replaceParam(Lambda(Array[Param](LambdaList(i).params(j)), LambdaList(i).body)
                , LambdaList(i).params(j), Param(TofJ))
              val F = FunCall(ir.ast.Map(L2), FunCallList(j1))
              F.t = ArrayType(LambdaList(i).body.t, eleLength)
              val Args = countParam(F)
              val L3 = Lambda(Args.toArray[Param], F)
              tempFunCallList += F
              tempLambdaList += L3
              Map_L_E += ((i, (2, j1)))
            }
          //}
          case _=>
        }
      }
      //}
    }
    val resLen = tempFunCallList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        FunCallList += tempFunCallList(randRes)
        LambdaList += tempLambdaList(randRes)
      }
    }
    else {
      FunCallList ++= tempFunCallList
      LambdaList ++= tempLambdaList
    }
  }
  private def matchZip(limitNum:Int,zipArrayLimit:Int):Unit ={
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    val ParamLength = ParamList.length
    val FunCallLength = FunCallList.length
    for(i <- 0 until ParamLength + FunCallLength){
      if((i>=Zip_P && i<ParamLength) || i>=ParamLength + Zip_F){
        val A0:Expr = i match{
          case i0 if i0 < ParamLength =>
            ParamList(i0)
          case i0 if i0 >= ParamLength =>
            FunCallList(i0 - ParamLength)
        }
        //1.A0 should have an arrayType
        A0.t match{
          case ArrayType(a0T,a0Len) =>
            val AId = scala.collection.mutable.ArrayBuffer[Int](i)
            for(j <- 0 until ParamLength + FunCallLength){
              val An:Expr = j match{
                case jn if jn < ParamLength =>
                  ParamList(jn)
                case jn if jn >= ParamLength =>
                  FunCallList(jn - ParamLength)
              }
              //2.An should be an arrayType and have the same length with A0
              An.t match {
                case ArrayType(_,`a0Len`) =>
                  AId += j
                case _ =>
              }
            }
            //we have AId : a list of array with the same length!
            //3.should have at lease two elements
            if(AId.length > 1){
              //Pass the type check!
              val randArgs = AId.length match{
                case temp1 if temp1 < zipArrayLimit =>
                  util.Random.nextInt(temp1)
                case _ =>
                  util.Random.nextInt(zipArrayLimit)
              }
              val Args = scala.collection.mutable.ArrayBuffer[Expr]()
              for(i <- 0 until randArgs){
                Args += getArg(AId(util.Random.nextInt(randArgs)))
              }
              //val F =FunCall(Zip(a0Len.eval), Args:_*)
              val F = FunCall(Zip(randArgs+1),Args:_*)
              val params = countParam(F)
              val L = Lambda(params.toArray[Param],F)
              TypeChecker(L)
              tempFunCallList += F
              tempLambdaList += L
            }
          case _ =>
        }
      }
    }
    Zip_P = ParamList.length
    Zip_F = FunCallList.length
    val resLen = tempFunCallList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        FunCallList += tempFunCallList(randRes)
        LambdaList += tempLambdaList(randRes)
      }
    }
    else {
      FunCallList ++= tempFunCallList
      LambdaList ++= tempLambdaList
    }

  }
  private def matchGet(limitNum:Int):Unit ={
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    val ParamLength = ParamList.length
    val FunCallLength = FunCallList.length
    for(i <- 0 until ParamLength + FunCallLength) {
      if ((i >= Get_P && i < ParamLength) || i >= ParamLength + Get_F) {
        val Arg:Expr = getArg(i)
        //Argument should have a tupletype
        Arg.t match{
          case tt: TupleType =>
            //Pass the type check!
            val F = FunCall(Get(util.Random.nextInt(tt.elemsT.length)),Arg)
            val params = countParam(F)
            val L = Lambda(params.toArray[Param],F)
            TypeChecker(L)
            tempFunCallList += F
            tempLambdaList += L

          case _=>
        }

      }
    }
    Get_P = ParamList.length
    Get_F = FunCallList.length
    val resLen = tempFunCallList.length
    if(resLen > limitNum){
      for(i <- 0 until limitNum){
        val randRes = util.Random.nextInt(resLen)
        FunCallList += tempFunCallList(randRes)
        LambdaList += tempLambdaList(randRes)
      }
    }
    else {
      FunCallList ++= tempFunCallList
      LambdaList ++= tempLambdaList
    }


  }
  /*This is a random-pick version,now we will use a enumeration version!
  private def matchJoin(): Unit = {
    val paramLength = ParamList.length
    val FunCallLength = FunCallList.length
    var resList = ArrayBuffer[Int]()
    for(i<- 0 until paramLength){
      _ = ParamList(i).t match{
        case ArrayType(ArrayType(t,m),n) =>
          resList+= i
        case _=>
      }
    }
    val paramCount = resList.length
    for(i<-0 until FunCallLength){
      _ = FunCallList(i).t match{
        case ArrayType(ArrayType(t,m),n) =>
          resList += i
        case _=>
      }
    }
    val totalCount = resList.length
    if(totalCount == 0){
      return (Param(),false)
    }
    val randChoice = util.Random.nextInt(totalCount)
    if(randChoice >= paramCount){
      return (FunCallList(randChoice-paramCount),true)
    }
    else{
      return (ParamList(randChoice),true)
    }
  }*/

}
*/
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



  //controllers for run programs
  val GlobalSize = 512
  val LocalSize = 32
  val RunInterpreter = false
  val useRandomRewrite = true
  val rewriteDepth = 5

  //controllers for generate programs
  val LoopNum = 35
  val consequentUserFun = false
  //val ReduceInitToGlobal = false
  val ReduceOnOneElement = false
  val AllowJoinASplit = false
  val MustContainsUserFun = true
  val MustContainsMap = true
  val StrictMatchUnpack = true
  var LimitNum = 40

  //controllers for patterns
  val ZipLimit = 2
  val SplitChunkSize = 4
  val GenJoin = true
  val GenSplit = true
  val GenUserFun = true
  val GenZip = false
  val GenGet = false
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

  //Zip
  var Zip_P = 0

  //Get
  var Get_P = 0

  //UnpackParam
  var UnPack_P = 0


  //Testing methods
  def trySingleLambda(l: Lambda, oril: Lambda, w: PrintWriter): Unit = {
    if(l.params.length > 7){
      println("too many argument, Ignored-by-user")
      writeln(w,"too many argument, Ignored-by-user")
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
          println("input type not inplemented, Ignored-by-user")
          writeln(w, "input type not inplemented, Ignored-by-user")
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
            println("OutPut Type unimplemented,Ignored-by-user")
            writeln(w, "OutPut Type unimplemented,Ignored-by-user")
            return
        }
      case false =>
        Array[Float](0)
    }
    //3. random rewriting
    val afterRewriting:scala.Seq[Lambda] = useRandomRewrite match {
      case true =>
        try {
          //Rewrite.rewriteWithoutLowering(l,rewriting.allRulesWithoutLowering,rewriteDepth)
          Rewrite.rewriteWithoutLowering (l, rewriting.allRulesWithoutMapsLowering (1, 1, 1), rewriteDepth)
        }
        catch {
          case e: Throwable =>
            println ("catch a exception in random-rewrite-by-user")
            e.printStackTrace ()
            writeln (w, "catch a exception in random-rewrite-by-user")
            e.printStackTrace (w)
            return
        }
      case false =>
        Seq(l)
    }
    for(rewriteId <- afterRewriting.indices) {

      //3. lower the lambda
      var fs = List[Lambda]()
      try {
        fs = Lower.mapCombinations(afterRewriting(rewriteId), new EnabledMappings(true, true, true, true, true, true))
      }
      catch {
        case e: Throwable =>
          println("catch a exception in lower-parser-by-user")
          e.printStackTrace()
          writeln(w, "catch a exception in lower-parser-by-user")
          e.printStackTrace(w)
          return
      }
      if (fs.length > 0) {
        for (lowId <- fs.indices) {

          val lowLevel = fs(lowId)
          if(lowLevel.isGenerable) {

            //4. compile the lambda
            var code = ""
            try {
              println(lowLevel.toString)
              writeln(w, lowLevel.toString)
              code = Compile(lowLevel)
            }
            catch {
              case e: Throwable =>
                println("catch a exception in compiler-by-user")
                writeln(w, "catch a exception in compiler-by-user")
                e.printStackTrace(w)
                return
            }

            //5. execute the OpenCL kernel
            try {
              //val (output_exe: Array[Float], runtime) = Execute(LocalSize, GlobalSize)(code, lowLevel, Args: _*)
              //val (output_exe: Array[Float], runtime1) = Execute(GlobalSize, GlobalSize)(code, lowLevel, Args: _*)

              var currGlobalSize = GlobalSize
              while(currGlobalSize >= 1) {
                var currLclSize = currGlobalSize
                while(currLclSize >= 1) {
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
                    writeln(w,"globalsize = " + currGlobalSize + " localsize = " + currLclSize)
                    writeln(w,"time = " + runtime)
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
      }
      else {
        println("No suitable lower-expressions, Ignored-by-user")
        writeln(w, "No suitable lower-expressions, Ignored-by-user")
      }
    }
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
  def generateProgram(): Unit = {
    //initial input..
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    ParamList += Param(ArrayType(Float,32))
    ParamList += Param(ArrayType(Float,32))
    ParamList += Param(Float)
    ParamList += Param(Float)


    val totalRounds = LoopNum
    for(i<- 0 until totalRounds){
      generateLambda()
      val test = LambdaList
      val test1 = ParamList
      val test2 = ParamToFunCall
      val test3 = UnpackedToExpr
    }
    refineResult()
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

    if(StrictMatchUnpack) {
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

  private def generateMap(limitNum:Int):Unit ={
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    //1. Search for proper lambda
    for( i<-LambdaList.indices) {

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
    if(StrictMatchUnpack){
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
  }
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