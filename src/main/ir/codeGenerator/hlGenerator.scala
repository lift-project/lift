package ir.codeGenerator

import collection.mutable._
import ir._
import ir.Type
import ir.ast._
import opencl.ir._

object hlGenerator{
  var ParamList: ArrayBuffer[Param] = new ArrayBuffer[Param]()
  var FunCallList: ArrayBuffer[FunCall] = new ArrayBuffer[FunCall]()
  var LambdaList: ArrayBuffer[Lambda] = new ArrayBuffer[Lambda]()

  //Used for debug
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

  //UserFun
  var Add_Check = scala.collection.mutable.Set[((Int,Int),(Int,Int))]()
  var Add_Check_FunCall = scala.collection.mutable.Set[Int]()

  def generateProgram(): Unit = {
    ParamList += Param(ArrayType(Float,1024))
    ParamList += Param(Float)
    ParamList += Param(Float)
    val totalRounds = 16
    for(i<- 0 until totalRounds){
     generateLambda()
      //val test = LambdaList
      //val test1 = FunCallList
    }
  }

  def replaceParam(p:Param,oldP:Param,newP:Param):Param ={
    if(p.eq(oldP)){
      newP
    }
    else{
      p
    }
  }
  def replaceParam(l: Lambda, oldP: Param, newP: Param) : Lambda = {
    val newBody = replaceParam(l.body, oldP, newP)
    val newParams = l.params.map((p) => replaceParam(p,oldP,newP))
    if (!newBody.eq(l.body) || (newParams, l.params).zipped.exists( (e1, e2) => !e1.eq(e2)) )
      Lambda(newParams,newBody)
    else
      l
  }
  def replaceParam(e: Expr, oldP: Param, newP: Param): Expr = {
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
    val totChoiceNum = 4
    val SplitChunkSize = 4
    val randChoice = util.Random.nextInt(totChoiceNum)
    randChoice match{
    //AssignedChoiceNum match{

      //Join
      case 0 =>
        matchJoin()
        AssignedChoiceNum += 1
      case 1 =>
        matchSplit(SplitChunkSize)
        AssignedChoiceNum += 1
      case 2 =>
        matchUserFun()
        AssignedChoiceNum += 1
      case 3 =>
        matchReduce()
        AssignedChoiceNum = 0


    }
  }
  private def matchReduce(): Unit ={
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
            //3. choose a Ele -> k, k!= j
            for(k <- LambdaList(i).params.indices){
              if(k!= j){
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
                              val F = FunCall(Reduce(L2), ParamList(j1), ParamList(k1))
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
                              val F = FunCall(Reduce(L2), ParamList(j1), FunCallList(k1))
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
                              val F = FunCall(Reduce(L2), FunCallList(j1), ParamList(k1))
                              F.t = ArrayType(TofJ, 1)
                              val Args = countParam(F)
                              val L3 = Lambda(Args.toArray[Param], F)
                              tempFunCallList += F
                              tempLambdaList += L3
                              Reduce_L_I_E += ((i, (2, j1), (1, k1)))
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
                              val F = FunCall(Reduce(L2), FunCallList(j1), FunCallList(k1))
                              F.t = ArrayType(TofJ, 1)
                              val Args = countParam(F)
                              val L3 = Lambda(Args.toArray[Param], F)
                              tempFunCallList += F
                              tempLambdaList += L3
                              Reduce_L_I_E += ((i, (2, j1), (2, k1)))
                            }
                          }
                        case _=>
                      }
                    }
                  }
                }
              }
            }
          }
        }
        if(!satisfied){
          Reduce_Lambda_Check += i
        }
      }
    }
    FunCallList++=tempFunCallList
    LambdaList++=tempLambdaList
  }
  private def matchJoin(): Unit = {
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
          val F = FunCall(new Join(),FunCallList(i))
          F.t = ArrayType(t,m*n)
          val Args = countParam(F)
          val L = Lambda(Args.toArray[Param],F)
          tempFunCallList += F
          tempLambdaList += L
        case _=>
      }
    }
    Join_F = FunCallList.length
    FunCallList++=tempFunCallList
    LambdaList++=tempLambdaList
  }
  private def matchSplit(ChunkSize:Int): Unit ={
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
    FunCallList++=tempFunCallList
    LambdaList++=tempLambdaList
  }
  private def matchUserFun():Unit ={
    val tempFunCallList = ArrayBuffer[FunCall]()
    val tempLambdaList = ArrayBuffer[Lambda]()
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    for(i1<- ParamList.indices){
      for(i2<- ParamList.indices){
        if(!Add_Check(((1,i1),(1,i2)))){
          if(ParamList(i1).t == Float && ParamList(i2).t == Float){
            val F = FunCall(add,ParamList(i1),ParamList(i2))
            F.t = Float
            val Args = countParam(F)
            val L =Lambda(Args.toArray[Param],F)
            tempFunCallList += F
            tempLambdaList += L
          }
          Add_Check += (((1,i1),(1,i2)))
          Add_Check += (((1,i2),(1,i1)))
        }
      }
      for(i2<- FunCallList.indices){
        if(!Add_Check_FunCall(i2) && !Add_Check(((1,i1),(2,i2)))){
          if(ParamList(i1).t == Float && FunCallList(i2).t == Float){
            val F = FunCall(add,ParamList(i1),FunCallList(i2))
            F.t = Float
            val Args = countParam(F)
            val L =Lambda(Args.toArray[Param],F)
            tempFunCallList += F
            tempLambdaList += L
            Add_Check_FunCall += i2
          }
          Add_Check += (((1,i1),(2,i2)))
          Add_Check += (((1,i2),(2,i1)))

        }
      }
    }
    for(i1<-FunCallList.indices){
      for(i2<-ParamList.indices){
        if(!Add_Check_FunCall(i1) && !Add_Check(((2,i1),(1,i2)))){
          if(FunCallList(i1).t == Float && ParamList(i2).t == Float){
            val F = FunCall(add,FunCallList(i1),ParamList(i2))
            F.t = Float
            val Args = countParam(F)
            val L =Lambda(Args.toArray[Param],F)
            tempFunCallList += F
            tempLambdaList += L
            Add_Check_FunCall += i1
          }
          Add_Check += (((2,i1),(1,i2)))
          Add_Check += (((2,i2),(1,i1)))

        }
      }
      for(i2<-FunCallList.indices){
        if(!Add_Check_FunCall(i1) && !Add_Check_FunCall(i2) && !Add_Check(((2,i1),(2,i2)))){
          if(FunCallList(i1).t == Float && FunCallList(i2).t == Float){
            val F = FunCall(add,FunCallList(i1),FunCallList(i2))
            F.t = Float
            val Args = countParam(F)
            val L =Lambda(Args.toArray[Param],F)
            tempFunCallList += F
            tempLambdaList += L
            Add_Check_FunCall += i1
            Add_Check_FunCall += i2
          }
          Add_Check += (((2,i1),(2,i2)))
          Add_Check += (((2,i2),(2,i1)))

        }
      }
    }
    FunCallList++=tempFunCallList
    LambdaList++=tempLambdaList
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