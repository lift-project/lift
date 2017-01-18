//Basicly, this high-level program generator generate Lambdas in a loop:

//First, it try to use options(i.e patterns, like join,split,map,reduce..) that takes the initial input (Note that FPatterns also need a Lambda as input)
//Each round, it will generate :
//A FunCall
//A Lambda using the funcall
//A param that have the same type with the lambda
//A param that unpack the result array(only when 1.the result type is an ArrayType 2.MapStrictMatchUnpack or ReduceStrictMatchUnpack or both of them are set as True)

//For example: if we have a Param x with x.t == ArrayType(Float,64),then it can generate:
//1. Split(8) $ x
//2. fun(ArrayType(Float,64),x =>{ Split(8) $ x})
//3. Param(ArrayType(ArrayType(Float,8),8)
//4. Param(ArrayType(8))

//Second, it will use the Params(and Lambdas for FPatterns) that comes form initial input or the first step, to generate deeper programs

//Then, repeatedly generate Lambdas using the Params and Lambdas we have, until reach the loop limit(also the depth limit)

//Finally, refine the result,deal with the Params that comes from FunCall or unpack

//The final result is stored in RefinedResult:ArrayBuffer[Lambda]

//Now we have Join, Split,UserFun, Zip,Get,Map,Reduce, and can generate the features of Matrix Mult
//It's simple to support more patterns.. Just tell me

//Then the usage of the controllers:


//1.LoopNum : Int                => The loop limit (max depth)
//2.ConsequentUserFun: Boolean   => allow for UserFun() o UserFun()
//3.ReduceOnOneElement: Boolean  => allow for reduction on only one element. The compiler have a problem with that.
//4.AllowJoinASplit: Boolean     => allow for Join()o Split(). It is identical
//5.MustContainsUserFun: Boolean => filter the result that does not contains any userfun
//6.MustContainsMap:Boolean      => filter the result that does not contains any maps
//7.MapStrictMatchUnpack:Boolean => When generate Map, like:
//                                  Map(Lambda1) o Param2, the input of Lambda1 is Param1
//                                  If MapStrictMatchUnpack is set to True, then it will ensure: Param1 comes form the unpack of Param2
//8.ReduceStrictMatchUnpack:Boolean  => similar with (7)
//9.LimitNum:Int                 => the max number of Lambda generated each cycle

//10.ZipLimit:Int                => Max number of Param to zip
//11.GenJoin/GenSplit... :Boolean=> Whether generate a certain pattern


package prog_gen

import ir._
import ir.ast._
import opencl.executor.Eval
import opencl.ir._
import opencl.ir.pattern.ReduceSeq

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProgramGenerator {

  var RefinedResult = mutable.Buffer[Lambda]()
  val ParamList = mutable.Buffer[Param]()
  val LambdaList = mutable.Buffer[Lambda]()
  val ParamToFunCall = collection.mutable.Map[Param, FunCall]()
  val UnpackedToExpr = collection.mutable.Map[Param,Expr]()

  //Used for debug
  var AssignedChoiceNum = 0
  val PassParamUpPossibility = 0.0

  //controllers for generate programs
  val LoopNum = 30
  val ConsequentUserFun = false
  val ReduceOnOneElement = false
  val AllowJoinASplit = false
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

  private val validReduction =
    Seq((add, FloatToValue(0.0f)), (mult, FloatToValue(1.0f)))

  //genrators
  def generatePrograms(): Array[Lambda] = {
    //initial input..
    // TODO: Non constant
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    ParamList += Param(ArrayType(ArrayType(Float,32),32))
    ParamList += Param(ArrayType(Float,32))
    ParamList += Param(ArrayType(Float,32))
    ParamList += Param(Float)
    ParamList += Param(Float)

    for (_ <- 0 until LoopNum)
      generateLambda()

    refineResult()
    filterIllegals()
    filterDuplicates()
    RefinedResult.toArray[Lambda]
  }

  private def filterIllegals(): Unit = {
    RefinedResult = RefinedResult.par.filter(program => {
      try {

        val newVersion = Eval(rewriting.utils.Utils.dumpLambdaToString(program))

        // TODO: Returning tuples is currently not supported, see issue #36
        !TypeChecker(newVersion).isInstanceOf[TupleType]

      } catch {
        case _: Throwable => false
      }
    }).toBuffer
  }

  private def filterDuplicates(): Unit = {

    val grouped = RefinedResult.groupBy(l =>
      rewriting.utils.Utils.Sha256Hash(rewriting.utils.Utils.dumpLambdaToString(l)))

    RefinedResult = grouped.map(_._2.head).toBuffer
  }

  private def generateLambda(): Unit = {
    val totChoiceNum = 7

    val randChoice = util.Random.nextInt(totChoiceNum)

    unpackParams()
    //randChoice match{
    // TODO: Composition of map and reduce?
    AssignedChoiceNum match {
      case 0 if GenJoin =>
        generateJoin()

      case 1 if GenSplit =>
        generateSplit()

      case 2 if GenUserFun =>
        generateUserFun(30)

      case 3 if GenZip =>
        generateZip()

      case 4 if GenGet =>
        generateGet()

      case 5 if GenMap =>
        generateMap()

      case 6 if GenReduce =>
        generateReduce()

        LimitNum += 10
    }

    AssignedChoiceNum = (AssignedChoiceNum + 1) % totChoiceNum
  }

  private def generateJoin(): Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    for(i <- Join_P until ParamList.length) {
      val param = ParamList(i)
      param.t match{
        case ArrayType(ArrayType(t,m),n) =>
          //pass the type check

          var joinSplit: Boolean = false
          if (ParamToFunCall.contains(param)) {
            ParamToFunCall(param).f match {
              case _:Split =>
                joinSplit = true
              case _=>
            }
          }

          if (!joinSplit || AllowJoinASplit) {
            //get the argument of FunCall
            val fArg = getArg(param,PassParamUpPossibility)

            //build the FunCall
            val F = FunCall(Join(), fArg)

            //set output type
            F.t = ArrayType(t, m * n)

            //build the param corresponds to the FunCall
            val P = Param(F.t)

            //count the parameters of lambda
            val lParams = collectUnboundParams(F)

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

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def generateSplit(): Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    for (i<- Split_P until ParamList.length) {
      val param = ParamList(i)
      param.t match {
        case ArrayType(t,n) if n.eval >= SplitChunkSize =>
          //Pass the type check!

          //get the argument of FunCall
          val fArg = getArg(param,PassParamUpPossibility)

          //build the FunCall
          val F = FunCall(Split(SplitChunkSize),fArg)

          //set output type
          F.t = ArrayType(ArrayType(t,SplitChunkSize),n /^ SplitChunkSize)

          //build the param corresponds to the FunCall
          val P = Param(F.t)

          //count the parameters of lambda
          val lParams = collectUnboundParams(F)

          //build the lambda
          val L = Lambda(lParams.toArray[Param],F)

          tempParamList += P
          tempLambdaList += L
          tempParamToFunCall += ((P,F))

        case _=>
      }
    }
    Split_P = ParamList.length

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def limitResults(tempLambdaList: ArrayBuffer[Lambda], tempParamList: ArrayBuffer[Param], tempParamToFunCall: mutable.Map[Param, FunCall], limitNum: Int = LimitNum) = {

    val resLen = tempParamList.length

    if (resLen > limitNum) {
      for (_ <- 0 until limitNum) {
        val randRes = util.Random.nextInt(resLen)
        if (!ParamToFunCall.contains(tempParamList(randRes))) {
          LambdaList += tempLambdaList(randRes)
          ParamList += tempParamList(randRes)
          ParamToFunCall += ((tempParamList(randRes), tempParamToFunCall(tempParamList(randRes))))
        }
      }
    } else {
      LambdaList ++= tempLambdaList
      ParamList ++= tempParamList
      ParamToFunCall ++= tempParamToFunCall
    }
  }

  private def generateUserFun(limitNum:Int):Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param, FunCall]()

    for (i1 <- ParamList.indices) {
      for (i2 <- ParamList.indices) {
        if (!Add_Check((i1, i2))) {
          val param1 = ParamList(i1)
          val param2 = ParamList(i2)

          if (param1.t == Float && param2.t == Float) {
            //check for consequentUserFun
            var containsUserFun = false
            if (ParamToFunCall.contains(param1)) {
              ParamToFunCall(param1).f match {
                case _: UserFun =>
                  containsUserFun = true
                case _ =>
              }
            }
            if (ParamToFunCall.contains(param2)) {
              ParamToFunCall(param2).f match {
                case _: UserFun =>
                  containsUserFun = true
                case _ =>
              }
            }

            //Don't allow for UserFun(UserFun(...))
            if (!containsUserFun || ConsequentUserFun) {
              val arg1 = getArg(param1, PassParamUpPossibility)
              val arg2 = getArg(param2, PassParamUpPossibility)

              val F = FunCall(add, arg1, arg2)
              F.t = Float
              val P = Param(F.t)
              //count the parameters of lambda
              val lParams = collectUnboundParams(F)

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

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall, limitNum)
  }

  // TODO: Only allow UserFun, Value combinations that are
  // TODO: an associative function and the neutral element.
  private def generateReduce(): Unit = {

    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param, FunCall]()

    def finishGenerateReduce(oriLambda: Lambda, initParamIndexOfLambda: Int, TofInit: Type, eleParamIndexOfLambda: Int, argInitIndex: Int, argEle: Expr) = {
      val lambda = Lambda(Array(oriLambda.params(initParamIndexOfLambda), oriLambda.params(eleParamIndexOfLambda)), oriLambda.body)
      val replace = FunDecl.replace(lambda,
        oriLambda.params(initParamIndexOfLambda), Param(oriLambda.params(initParamIndexOfLambda).t))
      //create new lambda for reduction(base on the original one)
      //only use two param for this lambda ,deal with other params outside
      val L2 = FunDecl.replace(
        replace,
        oriLambda.params(eleParamIndexOfLambda),
        Param(oriLambda.params(eleParamIndexOfLambda).t)
      )

      val param = ParamList(argInitIndex)
      //generate args
      val argInit: Expr = ParamToFunCall.getOrElse(param, param)

      val F = TofInit match {
        case t1 if t1 == oriLambda.params(eleParamIndexOfLambda).t => FunCall(Reduce(L2), argInit, argEle)
        case _ => FunCall(ReduceSeq(L2), argInit, argEle)
      }
      F.t = ArrayType(TofInit, 1)
      val P = Param(F.t)
      val Args = collectUnboundParams(F)
      val L3 = Lambda(Args.toArray[Param], F)

      tempLambdaList += L3
      tempParamList += P
      tempParamToFunCall += ((P, F))
    }

    //1. Search for proper Lambda
    for (oriLambdaIndex <- LambdaList.indices) {
      //flag satisfied means: this lambda "could be" a lambda for a reduction
      var satisfied = false
      //1. Have at least 2 params
      val oriLambda = LambdaList(oriLambdaIndex)
      if (oriLambda.params.length >= 2 && (!Reduce_Lambda_Check(oriLambdaIndex))) {
        //2. Exist a Init Init.t == L.t
        for (initParamIndexOfLambda <- oriLambda.params.indices) {
          val TofInit = oriLambda.params(initParamIndexOfLambda).t
          if (oriLambda.body.t == TofInit) {
            //3. choose a Ele, ele comes from unpack and ele != Init
            for (eleParamIndexOfLambda <- oriLambda.params.indices){

              //3. choose a Ele, Ele != Init
              if (eleParamIndexOfLambda != initParamIndexOfLambda &&
                (!ReduceStrictMatchUnpack ||
                  UnpackedToExpr.contains(oriLambda.params(eleParamIndexOfLambda)))) {

                //4. choose argInit, argInit.t == Init.t
                for (argInitIndex <- ParamList.indices) {
                  if (ParamList(argInitIndex).t == TofInit) {

                    val TofEle = oriLambda.params(eleParamIndexOfLambda).t
                    val argElems = if (ReduceStrictMatchUnpack)
                      Seq(UnpackedToExpr(oriLambda.params(eleParamIndexOfLambda)))
                    else
                      ParamList

                    if (!ReduceStrictMatchUnpack)
                      satisfied = true

                    argElems.foreach(argEle => {
                      argEle.t match {
                        //Don't do reductions on array with length 1
                        //The opencl generator causes bugs here
                        case ArrayType(_, eleLength) if eleLength.eval > 1 && ReduceStrictMatchUnpack =>
                          finishGenerateReduce(oriLambda, initParamIndexOfLambda, TofInit,
                            eleParamIndexOfLambda, argInitIndex, argEle)
                        case ArrayType(TofEle, eleLength) if eleLength.eval > 1=>

                          val argEleFromGet =
                            getArg(argEle.asInstanceOf[Param], PassParamUpPossibility)

                          val argEleIndex = ParamList.indexOf(argEle)

                          if (!Reduce_L_PI_PE((oriLambdaIndex, argInitIndex, argEleIndex))) {

                            finishGenerateReduce(oriLambda, initParamIndexOfLambda, TofInit,
                              eleParamIndexOfLambda, argInitIndex, argEleFromGet)

                            Reduce_L_PI_PE += ((oriLambdaIndex, argInitIndex, argEleIndex))
                          }

                        case _ =>
                      }
                    })

                  }
                }
              }
            }
          }

        }

        if (!satisfied)
          Reduce_Lambda_Check += oriLambdaIndex
      }
    }
    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def generateMap(): Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param, FunCall]()

    if (MapStrictMatchUnpack) {
      //1. Search for proper lambda
      for (i <- Map_Checked until LambdaList.length) {

        //the map must contains a userfun nested deep inside
        val oriLambda = LambdaList(i)

        if (oriLambda.body.isConcrete) {


          //2. choose one as the param. The param must comes from unpack
          val params = oriLambda.params
          params.filter(UnpackedToExpr.contains).foreach(parameter => {
            val TofParam = parameter.t
            val argEle = UnpackedToExpr(parameter)

            //create new lambda for map(base one the original one)
            //only use one param for this lambda ,deal with other params outside the map
            val L2 = FunDecl.replace(
              Lambda(Array[Param](parameter), oriLambda.body),
              parameter, Param(TofParam)
            )

            //build the funcall
            val F = FunCall(Map(L2), argEle)
            F.t = ArrayType(LambdaList(i).body.t, argEle.t.asInstanceOf[ArrayType].len)

            //build the param corresponds to the funcall
            val P = Param(F.t)

            //count the params
            val lParam = collectUnboundParams(F)
            val L3 = Lambda(lParam.toArray[Param], F)

            //TypeChecker(L3)


            tempLambdaList += L3
            tempParamList += P
            tempParamToFunCall += ((P, F))
          })
        }
      }
      Map_Checked = LambdaList.length

    } else {

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
            val param = ParamList(argIndex)
            param.t match {
              case ArrayType(TofParam, eleLength) =>

                //Pass the Type check!
                if (!Map_L_E((i, argIndex))) {

                  //create new lambda for map(base one the original one)
                  //only use one param for this lambda ,deal with other params outside the map
                  val L2 = FunDecl.replace(Lambda(Array[Param](oriLambda.params(paramIndexOfLambda)), oriLambda.body)
                    , oriLambda.params(paramIndexOfLambda), Param(TofParam))

                  //generate args
                  val argEle = getArg(param, PassParamUpPossibility)

                  //build the funcall
                  val F = FunCall(ir.ast.Map(L2), argEle)
                  F.t = ArrayType(LambdaList(i).body.t, eleLength)

                  //build the param corresponds to the funcall
                  val P = Param(F.t)

                  //count the params
                  val lParam = collectUnboundParams(F)
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

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def generateZip():Unit = {
    assert(ZipLimit >= 2)
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()


    for (i <- Zip_P until ParamList.length) {
      ParamList(i).t match{
        //1. A0 should have an arrayType
        case ArrayType(_,a0Len) =>

          //2. AId : id of params that have the same type with A0
          val AId = scala.collection.mutable.ArrayBuffer[Int](i)
          for(j <- ParamList.indices){
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
              case temp1 if temp1 < ZipLimit =>
                util.Random.nextInt(temp1 - 1) + 2
              case _ =>
                util.Random.nextInt(ZipLimit - 1) + 2
            }

            //get the argument of f
            val Args = scala.collection.mutable.ArrayBuffer[Expr](getArg(ParamList(AId(0)), PassParamUpPossibility))
            for(_ <- 0 until argNum - 1)
              Args += getArg(ParamList(AId(util.Random.nextInt(AId.length))), PassParamUpPossibility)

            //build the funcall
            val F = FunCall(Zip(argNum),Args:_*)

            //set the output type
            TypeChecker(F)

            //build the param corresponds to the FunCall
            val P = Param(F.t)

            //count the parameters of lambda
            val lParams = collectUnboundParams(F)

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

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def generateGet(): Unit = {
    val tempLambdaList = ArrayBuffer[Lambda]()
    val tempParamList = ArrayBuffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    ParamList.view(Get_P, ParamList.length).foreach(param => param.t match {
      case tt: TupleType =>

        //build the FunCall
        val F = FunCall(Get(util.Random.nextInt(tt.elemsT.length)),getArg(param,PassParamUpPossibility))

        //set the output type
        TypeChecker(F)

        //build the param corresponds to the FunCall
        val P = Param(F.t)

        //count the parameters of lambda
        val lParams = collectUnboundParams(F)

        //build the lambda
        val L = Lambda(lParams.toArray[Param], F)

        tempParamList += P
        tempLambdaList += L
        tempParamToFunCall += ((P, F))

      case _=>
    })

    Get_P = ParamList.length

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def unpackParams(): Unit = {
    val tempParamList = ArrayBuffer[Param]()
    for (i <- UnPack_P until ParamList.length) {
      val param = ParamList(i)
      param.t match {
        case ArrayType(t, _) =>
          val tempParam = Param(t)
          tempParamList += tempParam

          if (ReduceStrictMatchUnpack || MapStrictMatchUnpack)
            UnpackedToExpr += (tempParam -> getArg(param, 0))

        case _ =>
      }
    }
    ParamList ++= tempParamList
    UnPack_P = ParamList.length
  }

  //helper functions
  private def refineParamToFunCall(oriLambda: Lambda): Lambda = {
    val refineParamList = oriLambda.params.filter(ParamToFunCall.contains)

    if (refineParamList.nonEmpty) {
      var L2 = Lambda(refineParamList, oriLambda.body)

      //replace them with new param
      refineParamList.foreach(p =>
        L2 = FunDecl.replace(L2,p,Param(p.t)))

      //create a funcall for it
      val F = FunCall(L2, refineParamList.map(ParamToFunCall):_*)

      val lParam = collectUnboundParams(F)
      val L = Lambda(lParam.toArray[Param],F)

      refineParamToFunCall(L)
    } else {
      oriLambda
    }
  }

  private def refineUnpack(oriLambda:Lambda):Lambda={
    for(i <- oriLambda.params.indices){
      val param = oriLambda.params(i)

      if (UnpackedToExpr.contains(param)) {

        var L2 = Lambda(Array[Param](param),oriLambda.body)

        L2 = FunDecl.replace(L2,param,Param(param.t))

        val argEle = UnpackedToExpr(param)
        val F = FunCall(Map(L2), argEle)

        val lParam = collectUnboundParams(F)
        val L3 = Lambda(lParam.toArray[Param], F)
        return refineUnpack(L3)
      }
    }

    oriLambda
  }

  private def refineOneLambda(oriLambda:Lambda): Lambda = {
    for(i <- oriLambda.params.indices){
      val param = oriLambda.params(i)

      if (UnpackedToExpr.contains(param) || ParamToFunCall.contains(param))
        return refineOneLambda(refineUnpack(refineParamToFunCall(oriLambda)))
    }

    oriLambda
  }

  private def refineResult(): Unit =
    RefinedResult ++= LambdaList.map(refineOneLambda).filter(l =>
      (!MustContainsMap || l.toString.contains("Map")) && l.body.isConcrete)


  private def getArg(p: Param, possibility:Double): Expr = {
    if (ParamToFunCall.contains(p) &&
      util.Random.nextFloat() >= possibility) {
      //calculate the param here, return the corresponding
      ParamToFunCall(p)
    } else {
      p
    }
  }

  private def collectUnboundParams(L: Lambda): ArrayBuffer[Param] =
    (collectUnboundParams(L.body) -- L.params).distinct

  private def collectUnboundParams(Fc: FunCall): ArrayBuffer[Param] = {
    val rs = Fc.f match {
      case l: Lambda =>
        collectUnboundParams(l)
      case p: FPattern =>
        collectUnboundParams(p.f)
      case _=>
        ArrayBuffer[Param]()
    }

    rs ++= Fc.args.flatMap(collectUnboundParams)

    rs.distinct
  }

  private def collectUnboundParams(E: Expr): ArrayBuffer[Param] = {
    E match {
      case fc: FunCall =>
        collectUnboundParams(fc)
      case p: Param =>
        ArrayBuffer[Param](p)
    }
  }
}
