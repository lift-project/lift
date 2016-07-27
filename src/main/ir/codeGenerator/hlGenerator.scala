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

  def generateProgram(): Unit = {
    ParamList += Param(ArrayType(Float,1024))
    ParamList += Param(Float)
    ParamList += Param(Float)
    val totalRounds = 16
    for(i<- 0 until totalRounds){
     generateLambda()
    }
  }
  private def countParam(L:Lambda):ArrayBuffer[Param]={
    (countParam(L.body) -- L.params.toBuffer[Param]).distinct
  }
  private def countParam(p:Pattern):ArrayBuffer[Param]={
    p match{
      case red: ir.ast.Reduce =>
        countParam(red.f.body)
      case m: ir.ast.Map =>
        countParam(m.f.body)
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
    val totChoiceNum = 2
    val SplitChunkSize = 4
    val randChoice = util.Random.nextInt(totChoiceNum)
    randChoice match{

      //Join
      case 0 =>
        matchJoin()
      case 1 =>
        matchSplit(4)


    }
  }
  private def matchReduce(): Unit ={
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
                        case ArrayType(TofK,_) =>
                          //Pass the type check!!!!!!!!!!!!!!!
                          if(!Reduce_L_I_E((i,(1,j1),(1,k1)))) {
                            val L2 = FunDecl.replace(
                              FunDecl.replace(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                                LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                              LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                            val F = FunCall(Reduce(L2), ParamList(j1), ParamList(k1))
                            F.t = ArrayType(TofJ,1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            FunCallList += F
                            LambdaList += L3
                            Reduce_L_I_E += ((i,(1,j1),(1,k1)))
                          }


                        case _=>
                      }
                    }
                    for(k1 <- FunCallList.indices){
                      FunCallList(k1).t match{
                        case ArrayType(TofK,_) =>
                          //Pass the type check!!!!!!!!!!!
                          if(!Reduce_L_I_E((i,(1,j1),(1,k1)))) {
                            val L2 = FunDecl.replace(
                              FunDecl.replace(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                                LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                              LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                            val F = FunCall(Reduce(L2), ParamList(j1), FunCallList(k1))
                            F.t = ArrayType(TofJ,1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            FunCallList += F
                            LambdaList += L3
                            Reduce_L_I_E += ((i,(1,j1),(2,k1)))
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
                        case ArrayType(TofK,_) =>
                        //Pass the type check!!!!!!!!!!!!!!!
                          if(!Reduce_L_I_E((i,(1,j1),(1,k1)))) {
                            val L2 = FunDecl.replace(
                              FunDecl.replace(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                                LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                              LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                            val F = FunCall(Reduce(L2), FunCallList(j1), ParamList(k1))
                            F.t = ArrayType(TofJ,1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            FunCallList += F
                            LambdaList += L3
                            Reduce_L_I_E += ((i,(2,j1),(1,k1)))
                          }
                        case _=>
                      }
                    }
                    for(k1 <- FunCallList.indices){
                      FunCallList(k1).t match{
                        case ArrayType(TofK,_) =>
                        //Pass the type check!!!!!!!!!!!
                          if(!Reduce_L_I_E((i,(1,j1),(1,k1)))) {
                            val L2 = FunDecl.replace(
                              FunDecl.replace(Lambda(Array(LambdaList(i).params(j), LambdaList(i).params(k)), LambdaList(i).body),
                                LambdaList(i).params(j), Param(LambdaList(i).params(j).t)),
                              LambdaList(i).params(k), Param(LambdaList(i).params(k).t))
                            val F = FunCall(Reduce(L2), FunCallList(j1), FunCallList(k1))
                            F.t = ArrayType(TofJ,1)
                            val Args = countParam(F)
                            val L3 = Lambda(Args.toArray[Param], F)
                            FunCallList += F
                            LambdaList += L3
                            Reduce_L_I_E += ((i,(2,j1),(2,k1)))
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
  }
  private def matchJoin(): Unit = {
    for(i<- Join_P until ParamList.length){
      ParamList(i).t match{
        case ArrayType(ArrayType(t,m),n) =>
          //Pass the type check!
          val F = FunCall(new Join(),ParamList(i))
          F.t = ArrayType(t,m*n)
          val Args = countParam(F)
          val L = Lambda(Args.toArray[Param],F)
          FunCallList += F
          LambdaList += L
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
          FunCallList += F
          LambdaList += L
        case _=>
      }
    }
    Join_F = FunCallList.length
  }
  private def matchSplit(ChunkSize:Int): Unit ={
    for(i<- Split_P until ParamList.length){
      ParamList(i).t match{
        case ArrayType(t,n) =>
          if(n.eval >=ChunkSize){
            //Pass the type check!
            val F = FunCall(new Split(ChunkSize),ParamList(i))
            F.t = ArrayType(ArrayType(t,ChunkSize),n /^ ChunkSize)
            val Args = countParam(F)
            val L = Lambda(Args.toArray[Param],F)
            FunCallList += F
            LambdaList += L
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
            FunCallList += F
            LambdaList += L
          }
        case _ =>
      }
    }
    Split_F = FunCallList.length
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