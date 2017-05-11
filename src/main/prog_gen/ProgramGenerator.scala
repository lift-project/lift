// Basically, this high-level program generator generate Lambdas in a loop:

// First, it try to use options(i.e patterns, like join,split,map,reduce..) that takes the initial input (Note that FPatterns also need a Lambda as input)
// Each round, it will generate :
// A FunCall
// A Lambda using the funcall
// A param that have the same type with the lambda
// A param that unpack the result array(only when 1.the result type is an ArrayType 2.MapStrictMatchUnpack or ReduceStrictMatchUnpack or both of them are set as True)

// For example: if we have a Param x with x.t == ArrayType(Float,64),then it can generate:
// 1. Split(8) $ x
// 2. fun(ArrayType(Float,64),x =>{ Split(8) $ x})
// 3. Param(ArrayType(ArrayType(Float,8),8)
// 4. Param(ArrayType(8))

// Second, it will use the Params(and Lambdas for FPatterns) that comes form initial input or the first step, to generate deeper programs

// Then, repeatedly generate Lambdas using the Params and Lambdas we have, until reach the loop limit(also the depth limit)

// Finally, refine the result,deal with the Params that comes from FunCall or unpack

// The final result is stored in RefinedResult:mutable.Buffer[Lambda]

// Now we have Join, Split,UserFun, Zip,Get,Map,Reduce, and can generate the features of Matrix Mult
// It's simple to support more patterns.. Just tell me

// Then the usage of the controllers:


// 1.LoopNum : Int                => The loop limit (max depth)
// 2.ConsequentUserFun: Boolean   => allow for UserFun() o UserFun()
// 3.ReduceOnOneElement: Boolean  => allow for reduction on only one element. The compiler have a problem with that.
// 4.AllowJoinASplit: Boolean     => allow for Join()o Split(). It is identical
// 5.MustContainsUserFun: Boolean => filter the result that does not contains any userfun
// 6.MustContainsMap:Boolean      => filter the result that does not contains any maps
// 7.MapStrictMatchUnpack:Boolean => When generate Map, like:
//                                   Map(Lambda1) o Param2, the input of Lambda1 is Param1
//                                   If MapStrictMatchUnpack is set to True, then it will ensure: Param1 comes form the unpack of Param2
// 8.ReduceStrictMatchUnpack:Boolean  => similar with (7)
// 9.LimitNum:Int                 => the max number of Lambda generated each cycle

// 10.ZipLimit:Int                => Max number of Param to zip
// 11.GenJoin/GenSplit... :Boolean=> Whether generate a certain pattern


package prog_gen

import com.typesafe.scalalogging.Logger
import ir._
import ir.ast._
import lift.arithmetic.{ArithExpr, Cst, SizeVar}
import opencl.executor.Eval
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import rewriting.utils.NumberExpression

import scala.collection.mutable

class ProgramGenerator(val loopNum: Int = 30, var limitNum: Int = 40) {

  private val logger = Logger(this.getClass)

  private[prog_gen] var RefinedResult = mutable.Buffer[Lambda]()
  private[prog_gen] val ParamList = mutable.Buffer[Param]()
  private[prog_gen] val LambdaList = mutable.Buffer[Lambda]()
  private[prog_gen] val ParamToFunCall = mutable.Map[Param, FunCall]()
  private[prog_gen] val UnpackedToExpr = mutable.Map[Param,Expr]()

  // Used for debug
  private var AssignedChoiceNum = 0
  private val PassParamUpPossibility = 0.0

  // Controllers for generate programs
  private val AllowJoinASplit = false
  private val MustContainMap = true

  // TODO: Do we need the StrictMatchUnpack?
  // TODO: Does it matter if types already match?
  private val MapStrictMatchUnpack = true
  private val ReduceStrictMatchUnpack = true

  // Controllers for patterns
  private val ZipLimit = 2
  private val GenJoin = true
  private val GenSplit = true
  private val GenUserFun = true
  private val GenZip = true
  private val GenGet = true
  private val GenMap = true
  private val GenReduce = true

  if (ZipLimit < 2)
    throw new IllegalArgumentException(s"ZipLimit of $ZipLimit needs to be larger")

  // Avoid considering same expressions again
  // Join
  private var Join_P = 0

  // Split
  private var Split_P = 0

  // Reduce
  private val Reduce_L_PI_PE = mutable.Set[(Int, Int, Int)]()
  private val Reduce_Lambda_Check = mutable.Set[Int]()

  // UserFun
  private val UserFun_Check = mutable.Set[Seq[Param]]()

  // Map
  private val Map_L_E = mutable.Set[(Int, Int)]()
  private var Map_Checked = 0

  // Zip
  private var Zip_P = 0

  // Get
  private var Get_P = 0

  // UnpackParam
  private var UnPack_P = 0

  private val validReduction =
    Seq((add, FloatToValue(0.0f)), (mult, FloatToValue(1.0f)))

  private val arrayLengths = Seq[ArithExpr](SizeVar("N"), SizeVar("M"))

  private val userFuns = Seq(add, mult)

  private val depthCutoff = 6

  def generatePrograms(): Array[Lambda] = {
    // Initial input TODO: enable all lengths
    val length = arrayLengths.head
    ParamList += Param(ArrayTypeWSWC(ArrayTypeWSWC(Float, length), length))
    ParamList += Param(ArrayTypeWSWC(ArrayTypeWSWC(Float, length), length))
    ParamList += Param(ArrayTypeWSWC(Float, length))
    ParamList += Param(ArrayTypeWSWC(Float, length))
    ParamList += Param(Float)
    ParamList += Param(Float)

    generateTranspose()

    for (_ <- 0 until loopNum)
      generateLambda()

    refineResult()
    filterIllegals()
    filterDuplicates()
    RefinedResult.toArray
  }

  private def filterIllegals(): Unit = {

    // TODO: Should I filter out useless Zips where not all components used?
    RefinedResult = RefinedResult.par.filter(program => {
      try {
        val quickCheck =
            // Don't allow tuples containing arrays as a single parameter
            program.params.forall(_.t match {
              case TupleType(tts@_*) => !tts.exists(_.isInstanceOf[ArrayType])
              case _ => true
            }) && NumberExpression.byDepth(program).values.max <= depthCutoff

        if (quickCheck) {
          // TODO: Quicker way of rebuilding expressions and getting rid
          // TODO: of sharing components? And making sure it's legal
          val newProgram = Eval(rewriting.utils.Utils.dumpLambdaToString(program))
          // TODO: Returning tuples is currently not supported, see issue #36
          !TypeChecker(newProgram).isInstanceOf[TupleType]
        } else {
          false
        }

      } catch {
        case t: TypeException =>
          logger.warn(s"Failed type-checking\n$program\nwith\n$t")
          false
        case t: Throwable =>
          logger.warn(s"Failed rebuilding\n$program\nwith\n$t")
          false
      }
    }).toBuffer
  }

  private def filterDuplicates(): Unit = {

    val grouped = RefinedResult.groupBy(l =>
      rewriting.utils.Utils.Sha256Hash(rewriting.utils.Utils.dumpLambdaToString(l)))

    RefinedResult = grouped.map(_._2.head).toBuffer
  }

  private def generateTranspose(): Unit = {
    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    ParamList.foreach(param => {
      param.t match{
        case ArrayTypeWSWC(ArrayTypeWSWC(_,_, _),_, _) =>

          // Get the argument of FunCall
          val arg = getArg(param,PassParamUpPossibility)

          // Build the FunCall
          val F = FunCall(Transpose(), arg)

          // Set output type
          TypeChecker(F)

          // Build the param corresponds to the FunCall
          val P = Param(F.t)

          // Count the parameters of lambda
          val lParams = collectUnboundParams(F)

          // Build the lambda
          val L = Lambda(lParams.toArray[Param], F)

          tempParamList += P
          tempLambdaList += L
          tempParamToFunCall += ((P, F))
        case _=>
      }
    })

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def generateLambda(): Unit = {
    val totChoiceNum = 7

    unpackParams()

    AssignedChoiceNum match {
      case 0 if GenJoin =>
        generateJoin()

      case 1 if GenSplit =>
        generateSplit()

      case 2 if GenZip =>
        generateZip()

      case 3 if GenGet =>
        generateGet()

      case 4 if GenUserFun =>
        generateUserFun()

      case 5 if GenMap =>
        generateMap()

      case 6 if GenReduce =>
        generateReduce()

        limitNum += 10
    }

    AssignedChoiceNum = (AssignedChoiceNum + 1) % totChoiceNum
  }

  private def generateJoin(): Unit = {
    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    for(i <- Join_P until ParamList.length) {
      val param = ParamList(i)
      param.t match{
        case ArrayTypeWSWC(ArrayTypeWSWC(t,ms,mc),ns,nc) if ms==mc && ns==nc =>
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
            // Get the argument of FunCall
            val fArg = getArg(param,PassParamUpPossibility)

            // Build the FunCall
            val F = FunCall(Join(), fArg)

            // Set output type
            F.t = ArrayTypeWSWC(t, ms * ns, mc * nc)

            // Build the param corresponds to the FunCall
            val P = Param(F.t)

            // Count the parameters of lambda
            val lParams = collectUnboundParams(F)

            // Build the lambda
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
    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    for (i <- Split_P until ParamList.length) {
      val param = ParamList(i)
      param.t match {
        case t@ArrayTypeWSWC(_,ms,mc) if ms==mc =>

          val chunkSize = rewriting.utils.Utils.validSplitVariable(t)

          // Get the argument of FunCall
          val fArg = getArg(param,PassParamUpPossibility)

          // Build the FunCall
          val F = FunCall(Split(chunkSize),fArg)

          // Set output type
          TypeChecker(F)

          // Build the param corresponds to the FunCall
          val P = Param(F.t)

          // Count the parameters of lambda
          val lParams = collectUnboundParams(F)

          // Build the lambda
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

  private def limitResults(tempLambdaList: mutable.Buffer[Lambda], tempParamList: mutable.Buffer[Param], tempParamToFunCall: mutable.Map[Param, FunCall], limitNum: Int = limitNum) = {

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

  private def generateUserFun(limitNum:Int = 30): Unit = {
    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
    val tempParamToFunCall = mutable.Map[Param, FunCall]()
    val tempUserFunCheck = mutable.Set[Seq[Param]]()

    userFuns.foreach(userFunToUse => {

      val inputTypes = userFunToUse.inTs
      val numArgs = inputTypes.length

      val checked = mutable.Set[Seq[Int]]()

      var parameterCombinations = Seq[Seq[Param]]()

      // TODO: Filter out equivalent ones?
      while (parameterCombinations.length < limitNum && checked.size < ParamList.length) {

        val indices = Seq.fill(numArgs)(util.Random.nextInt(ParamList.length))

        if (!checked.contains(indices)) {

          checked += indices

          val combination = indices.map(ParamList.apply)

          val typesMatch =
            !UserFun_Check.contains(combination) &&
              (combination, inputTypes).zipped.forall((p, t) => p.t == t)

          if (typesMatch)
            parameterCombinations = parameterCombinations :+ combination
        }

      }

      parameterCombinations.foreach(params => {

        val args = params.map(getArg(_, PassParamUpPossibility))

        val F = FunCall(userFunToUse, args: _*)
        F.t = userFunToUse.outT
        val P = Param(F.t)

        // Collect parameters for the lambda
        val lParams = collectUnboundParams(F)

        // Build the lambda
        val L = Lambda(lParams.toArray, F)

        tempUserFunCheck += params

        tempParamList += P
        tempLambdaList += L
        tempParamToFunCall += ((P, F))
      })
    })

    // Only add now, so all UserFun-s could be generated
    // if they have the same argument requirements
    UserFun_Check ++= tempUserFunCheck

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall, limitNum)
  }

  private def generateReduce(): Unit = {

    validReduction.foreach(pair => {
      val (function, init) = pair

      val reduce = Reduce(function)

      val typeToFind = function.outT

      val candidates = ParamList.filter(_.t match {
        case ArrayType(e) => e == typeToFind
        case _ => false
      })

      val calls = candidates.map(param =>
        FunCall(reduce, init, getArg(param, PassParamUpPossibility)))

      calls.foreach(TypeChecker(_))

      val params = calls.map(call => Param(call.t))

      val lambdaParameters = calls.map(collectUnboundParams)

      val lambdas = (lambdaParameters, calls).zipped.map((params, call) =>
        Lambda(params.toArray, call))

      // TODO: Limit after all generated?
      limitResults(lambdas, params, mutable.Map((params, calls).zipped.toSeq:_*))
    })
  }


  // TODO: Keep? Can generate more complex stuff.
  private def generateReduceSeq(): Unit = {

    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
    val tempParamToFunCall = mutable.Map[Param, FunCall]()

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

      val F = FunCall(ReduceSeq(L2), argInit, argEle)
      F.t = ArrayTypeWSWC(TofInit, 1)
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
                        case ArrayTypeWS(_, eleLength) if eleLength != Cst(1) && ReduceStrictMatchUnpack =>
                          finishGenerateReduce(oriLambda, initParamIndexOfLambda, TofInit,
                            eleParamIndexOfLambda, argInitIndex, argEle)
                        case ArrayTypeWS(TofEle, eleLength) if eleLength != Cst(1) =>

                          val argEleFromGet =
                            getArg(argEle.asInstanceOf[Param], PassParamUpPossibility)

                          val argEleIndex = ParamList.indexOf(argEle)

                          if (!Reduce_L_PI_PE((oriLambdaIndex, argInitIndex, argEleIndex))) {

                            finishGenerateReduce(oriLambda, initParamIndexOfLambda, TofInit,
                              eleParamIndexOfLambda, argInitIndex, argEleFromGet)

                            Reduce_L_PI_PE += ((oriLambdaIndex, argInitIndex, argEleIndex))
                          }

                        case _ : ArrayType => throw new NotImplementedError()
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
    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
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
              parameter,
              Param(TofParam)
            )

            //build the funcall
            val F = FunCall(Map(L2), argEle)
            F.t = ArrayTypeWSWC(oriLambda.body.t,
              argEle.t.asInstanceOf[ArrayType with Size with Capacity].size,
              argEle.t.asInstanceOf[ArrayType with Size with Capacity].capacity
            )

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

        if (oriLambda.body.isConcrete) {

          val params = oriLambda.params
          //2. choose one as the param
          val paramIndexOfLambda = util.Random.nextInt(params.length)

          val parameter = params(paramIndexOfLambda)
          //for (j <- LambdaList(i).params.indices){
          //Get the type of it
          val TofParam = parameter.t

          //3. search for a proper Arg.t == ArrayType(TofParam)
          for (argIndex <- ParamList.indices) {
            val param = ParamList(argIndex)
            param.t match {
              case ArrayTypeWSWC(TofParam, eleLength, eleCap) if eleLength == eleCap =>

                //Pass the Type check!
                if (!Map_L_E((i, argIndex))) {

                  //create new lambda for map(base one the original one)
                  //only use one param for this lambda ,deal with other params outside the map
                  val L2 = FunDecl.replace(
                    Lambda(Array[Param](parameter), oriLambda.body),
                    parameter,
                    Param(TofParam)
                  )

                  //generate args
                  val argEle = getArg(param, PassParamUpPossibility)

                  //build the funcall
                  val F = FunCall(Map(L2), argEle)
                  F.t = ArrayTypeWSWC(oriLambda.body.t, eleLength)

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

              case _: ArrayType =>
                throw new NotImplementedError()
              case _ =>
            }
          }
        }
      }
    }

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def generateZip(): Unit = {
    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    for (i <- Zip_P until ParamList.length) {
      ParamList(i).t match {
        //1. A0 should have an arrayType
        case ArrayTypeWS(_,a0Len) =>

          //2. AId : id of params that have the same type with A0
          val AId = mutable.Buffer[Int](i)

          // TODO: Zipping split arrays with non-constant length and chunk size?
          for(j <- ParamList.indices){
            ParamList(j).t match {
              // Zipping the same thing twice is useless
              case ArrayTypeWS(_,`a0Len`) if i != j => AId += j
              case _ =>
            }
          }

          //3. should have at least 2 elements
          if (AId.length >= 2) {

            // TODO: Can still pick the same thing twice, because of the random picking
            //randomly choose 'argNum' of params from AId
            val argNum = AId.length match {
              case temp1 if temp1 < ZipLimit =>
                util.Random.nextInt(temp1 - 1) + 2
              case _ =>
                util.Random.nextInt(ZipLimit - 1) + 2
            }

            //get the argument of f
            val Args = mutable.Buffer[Expr](getArg(ParamList(AId(0)), PassParamUpPossibility))
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
    val tempLambdaList = mutable.Buffer[Lambda]()
    val tempParamList = mutable.Buffer[Param]()
    val tempParamToFunCall = collection.mutable.Map[Param,FunCall]()

    ParamList.view(Get_P, ParamList.length).foreach(param => param.t match {
      case tt: TupleType =>

        // Build the FunCalls
        val calls = tt.elemsT.indices.map(i => FunCall(Get(i), param))

        // Set the output types
        calls.foreach(TypeChecker.apply)

        // Build the param corresponds to the FunCalls
        val params = calls.map(f => Param(f.t))

        // Collect the parameters of lambdas
        val lambdaParams = calls.map(collectUnboundParams)

        // Build the lambdas
        val Ls = (calls, lambdaParams).zipped.map((f, p) => Lambda(p.toArray, f))

        tempParamList ++= params
        tempLambdaList ++= Ls
        tempParamToFunCall ++= (params, calls).zipped.toSeq

      case _=>
    })

    Get_P = ParamList.length

    limitResults(tempLambdaList, tempParamList, tempParamToFunCall)
  }

  private def unpackParams(): Unit = {
    val tempParamList = mutable.Buffer[Param]()
    for (i <- UnPack_P until ParamList.length) {
      val param = ParamList(i)
      param.t match {
        case ArrayType(t) =>
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

  // Helper functions
  private def refineParamToFunCall(oriLambda: Lambda): Lambda = {
    val refineParamList = oriLambda.params.filter(ParamToFunCall.contains)

    if (refineParamList.nonEmpty) {
      var L2 = Lambda(refineParamList, oriLambda.body)

      // Replace them with new param
      refineParamList.foreach(p =>
        L2 = FunDecl.replace(L2,p,Param(p.t)))

      // Create a funcall for it
      val F = FunCall(L2, refineParamList.map(ParamToFunCall):_*)

      val lParam = collectUnboundParams(F)
      val L = Lambda(lParam.toArray[Param],F)

      refineParamToFunCall(L)
    } else {
      oriLambda
    }
  }

  private def refineUnpack(oriLambda:Lambda): Lambda = {
    for (i <- oriLambda.params.indices) {
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
    for (i <- oriLambda.params.indices) {
      val param = oriLambda.params(i)

      if (UnpackedToExpr.contains(param) || ParamToFunCall.contains(param))
        return refineOneLambda(refineUnpack(refineParamToFunCall(oriLambda)))
    }

    oriLambda
  }

  private def refineResult(): Unit =
    RefinedResult ++= LambdaList.map(refineOneLambda).filter(l =>
      (!MustContainMap || l.toString.contains("Map")) && l.body.isConcrete)


  private def getArg(p: Param, possibility: Double): Expr = {
    if (ParamToFunCall.contains(p) &&
      util.Random.nextFloat() >= possibility) {
      //calculate the param here, return the corresponding
      ParamToFunCall(p)
    } else {
      p
    }
  }

  private def collectUnboundParams(L: Lambda): mutable.Buffer[Param] =
    (collectUnboundParams(L.body) -- L.params).distinct

  private def collectUnboundParams(Fc: FunCall): mutable.Buffer[Param] = {
    val rs = Fc.f match {
      case l: Lambda => collectUnboundParams(l)
      case p: FPattern => collectUnboundParams(p.f)
      case _=> mutable.Buffer[Param]()
    }

    rs ++= Fc.args.flatMap(collectUnboundParams)

    rs.distinct
  }

  private def collectUnboundParams(E: Expr): mutable.Buffer[Param] = {
    E match {
      case fc: FunCall => collectUnboundParams(fc)
      case _: Value => mutable.Buffer[Param]()
      case p: Param => mutable.Buffer[Param](p)
    }
  }
}
