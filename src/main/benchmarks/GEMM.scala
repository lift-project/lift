package benchmarks

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.executor.Utils
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class GEMM (override val f: Seq[(String, Array[Lambda])])
  extends DeprecatedBenchmark("Matrix Multiplication", Seq(1024, 1024, 1024), f, 0.1f, Array(16, 16, 1)) {
  // Parser options
//  case class GEMMConfig(trials: Int = 10,
//                        platform: Int = 0,
//                        device: Int = 0,
//                        saveOutput: Option[String] = None,
//                        loadOutput: Option[String] = None,
//                        localSize: Array[Int] = defaultLocalSizes,
//                        globalSize: Array[Int] = Array(inputSizes().product, 1, 1),
//                        loadKernel: Option[String] = None,
//                        saveAll: Boolean = false,
//                        loadAll: Boolean = false,
//                        csvFileName: Option[String] = None,
//                        size: List[Int] = null,
//                        verbose: Boolean = false,
//                        variant: Int = 0,
//                        variantRegex: String = ".*",
//                        negateRegex: Boolean = false,
//                        all: Boolean = false,
//                        timeout: Option[Int] = None,
//                        checkResult: Boolean = false,
//                        printResult: Boolean = false,
//                        injectLocal: Boolean = false,
//                        injectGroup: Boolean = false,
//                        experimentID: Option[String] = None,
//                        input: File = null,
//                        tileX: Long = 0,
//                        tileY: Long = 0,
//                        registerBlockM: Long = 0,
//                        registerBlockN: Long = 0)
//
//  val gemmBuilder: OParserBuilder[GEMMConfig] = OParser.builder[GEMMConfig]
//  var gemmCmdArgs: Option[GEMMConfig] = None
//  val gemmParser = {
//    import gemmBuilder._
//    OParser.sequence(
//      programName("Benchmark"),
//      opt[Int]('i', "trials").text("Total trials (Default: 10)")
//        .foreach(arg => c =c.copy(trials = arg)),
//
//      opt[Int]('p', "platform").text("Id of the OpenCL platform to use (Default: 0)")
//        .foreach(arg => c =c.copy(platform = arg)),
//
//      opt[Int]('d', "device").text("Id of the OpenCL device to use (Default: 0)")
//        .foreach(arg => c =c.copy(device = arg)),
//
//      opt[String]("saveOutput").text("Save the gold result of the computation to a file")
//        .foreach(arg => c =c.copy(saveOutput = Some(arg))),
//
//      opt[String]("loadOutput").text("Load the gold result of the computation from a file. Takes precedence over saveOutput")
//        .foreach(arg => c =c.copy(loadOutput = Some(arg))),
//
//      opt[Seq[Int]]('l', "localSize").text("Local size(s) to use " +
//        "(Defaults: " + defaultLocalSizes.mkString(", ") + ")")
//        .foreach(arg => c =c.copy(localSize = arg.toArray))
//        .minOccurs(3).maxOccurs(3),
//
//      opt[Seq[Int]]('g', "globalSize").text("Global size(s) to use")
//        .foreach(arg => c =c.copy(globalSize = arg.toArray))
//        .minOccurs(3).maxOccurs(3),
//
//      opt[String]("loadKernel").text("Load an OpenCL kernel source file")
//        .foreach(arg => c =c.copy(loadKernel = Some(arg))),
//
//      // TODO: Implement kernel saving/loading functionality.
//      opt[Unit]("save-kernels").text("Save all kernels.")
//        .action((_, c) => c.copy(saveAll = true)),
//
//      opt[Unit]("load-kernels").text("Load kernel for execution previously generated using -save-kernels")
//        .action((_, c) => c.copy(loadAll = true)),
//
//      opt[String]("csvFileName").abbr("csv").text("If specified, results are stored in .csv file with given name")
//        .foreach(arg => c =c.copy(csvFileName = Some(arg))),
//
//      opt[Seq[Int]]('s', "inputSize").text("Size of the input to use, expecting ${defaultInputSizes.length}%d sizes.")
//        .required()
//        .foreach(arg => c =c.copy(size = arg.toList))
//        .minOccurs(defaultInputSizes.length).maxOccurs(defaultInputSizes.length),
//
//      opt[Unit]('v', "verbose").text("Print allocated memory and source code")
//        .action((_, c) => c.copy(verbose = true)),
//
//      opt[Int]("variant").abbr("var").text("Which of the following variants to run (Default: 0):\n" +
//        f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))
//        .foreach(arg => c =c.copy(variant = arg)),
//
//      opt[String]("variant-regex").abbr("var-regex").text("Which variant(s) to run, based on a regular expression")
//        .foreach(arg => c =c.copy(variantRegex = arg)),
//
//      opt[Unit]("negate-regex").abbr("nr").text("Negate the regular expression matching variants, " +
//        "i.e. only select variants which do not match the regex")
//        .action((_, c) => c.copy(negateRegex = true)),
//
//      opt[Unit]('a', "all").text("Run all variants, takes precedence over the variant option.")
//        .action((_, c) => c.copy(all = true)),
//
//      opt[Int]('t', "timeout").text("If the kernel execution is longer than time, ignore any remaining trials.")
//        .foreach(arg => c =c.copy(timeout = Some(arg))),
//
//      opt[Unit]('c', "check").text("Check the result")
//        .action((_, c) => c.copy(checkResult = true)),
//
//      opt[Unit]("print").text("Print the result")
//        .action((_, c) => c.copy(printResult = true)),
//
//      opt[Unit]("inject").abbr("il").text("Inject the local size into the kernel as a constant, " +
//        "possibly replacing some for loops with if statements.")
//        .action((_, c) => c.copy(injectLocal = true)),
//
//      opt[Unit]("injectGroup").abbr("ig").text("Inject the number of groups into the kernel as a constant, " +
//        "possibly replacing some for loops with if statements.")
//        .action((_, c) => c.copy(injectGroup = true)),
//
//      opt[String]("experimentId").abbr("eid").text("A unique ID for this experiement for reporting data")
//        .foreach(arg => c =c.copy(experimentID = Some(arg))),
//
//      opt[File]("input").text("Input files to read")
//        .required()
//        .foreach(arg => c =c.copy(input = arg))
//        .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist")),
//
//      opt[Int]('x', "tileX").text("Tile size in the M and N dimension").required()
//        .foreach(arg => c =c.copy(tileX = arg)),
//
//      opt[Int]('y', "tileY").text("Tile size in the K dimension").required()
//        .foreach(arg => c =c.copy(tileY = arg)),
//
//      opt[Int]("blockM").abbr("bm").text("Register blocking factor in M dimension").required()
//        .foreach(arg => c =c.copy(registerBlockM = arg)),
//
//      opt[Int]("blockN").abbr("bn").text("Register blocking factor in N dimension").required()
//        .foreach(arg => c =c.copy(registerBlockN = arg)),
//
//      help("help").text("Show this message.")
//    )}

  case class GEMMConfig(tileX: Option[Long] = None,
                        tileY: Option[Long] = None,
                        registerBlockM: Option[Long] = None,
                        registerBlockN: Option[Long] = None)


    var gemmCmdArgs = GEMMConfig()
    val gemmParser = new scopt.OptionParser[Unit]("GEMM") {
      override val errorOnUnknownArgument = false

      opt[Long]('x', "tileX").text("Tile size in the M and N dimension").required()
        .foreach(arg => gemmCmdArgs = gemmCmdArgs.copy(tileX = Some(arg)))

      opt[Long]('y', "tileY").text("Tile size in the K dimension").required()
        .foreach(arg => gemmCmdArgs = gemmCmdArgs.copy(tileY = Some(arg)))

      opt[Long]("blockM").abbr("bm").text("Register blocking factor in M dimension").required()
        .foreach(arg => gemmCmdArgs = gemmCmdArgs.copy(registerBlockM = Some(arg)))

      opt[Long]("blockN").abbr("bn").text("Register blocking factor in N dimension").required()
        .foreach(arg => gemmCmdArgs = gemmCmdArgs.copy(registerBlockN = Some(arg)))
    }


  override def runScala(inputs: Any*): Array[Float] = {
    var A = inputs(0).asInstanceOf[Array[Array[Float]]]
    val B = inputs(1).asInstanceOf[Array[Array[Float]]]
    val C = inputs(2).asInstanceOf[Array[Array[Float]]]
    val alpha = inputs(3).asInstanceOf[Float]
    val beta = inputs(4).asInstanceOf[Float]

    val variant = cmdArgs.variant
    if (variant != 0)
      A = A.transpose

    val res = Utils.matrixMatrixMultiply(A, B, C, alpha, beta)

    res.flatten
  }



  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes().head
    val inputSizeM = inputSizes()(1)
    val inputSizeK = inputSizes()(2)

    val matrixA = Array.tabulate(inputSizeM, inputSizeK)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val matrixB = Array.tabulate(inputSizeK, inputSizeN)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val matrixC = Array.tabulate(inputSizeK, inputSizeN)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)

    Seq(matrixA, matrixB, matrixC, 1.5f, 2.5f)
  }

  override def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes().head, inputSizes()(1), 1)
    cmdArgs.globalSize.copyToArray(globalSizes)
    globalSizes
  }

  override protected def beforeBenchmark(): Unit = {
    val temp = f(1)._2
    temp(0) = GEMM.tiledAndBlockedBInnermost(Cst(gemmCmdArgs.tileX.getOrElse(16)),
      Cst(gemmCmdArgs.tileX.getOrElse(16)), Cst(gemmCmdArgs.tileY.getOrElse(8)), Cst(gemmCmdArgs.registerBlockN.getOrElse(4)),
      Cst(gemmCmdArgs.registerBlockM.getOrElse(4)))
  }

  override protected def printParams(): Unit = {
    println("Tile size: " + gemmCmdArgs.tileX.getOrElse(16) + " " + gemmCmdArgs.tileY.getOrElse(8))
    println("Work per thread: " + gemmCmdArgs.registerBlockN.getOrElse(4) + " " + gemmCmdArgs.registerBlockM.getOrElse(4))
  }

  override protected def printResults(time: Double): Unit = {
    val dNumOps = 2.0 * inputSizes().head.toDouble * inputSizes()(1).toDouble * inputSizes()(2).toDouble
    val gflops = 1.0e-6 * dNumOps/time
    println("THROUGHPUT: " + gflops + " GFlops/s")
  }
}

object GEMM {
  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  val naive = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    Float,
    Float,
    (A, B, C, alpha, beta) => {
      MapGlb(fun( aRow =>
        Join() o  MapSeq(fun( bCol =>
          toGlobal(MapSeq(id)) o
            MapSeq(fun(x => multAndSumUp(x, beta, Get(bCol, 1)))) o
            MapSeq(fun(x => mult(x, alpha))) o
            ReduceSeq(fun((acc, y) =>
              multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
            ), 0.0f) $ Zip(Get(aRow, 0), Get(bCol, 0))
        )) $ Zip(Transpose() $ B, Get(aRow, 1))
      )) $ Zip(A, C)
    })

  // Currently the best for NVIDIA
  def tiledAndBlockedBInnermost(tileSizeN: ArithExpr, tileSizeM: ArithExpr, tileSizeK: ArithExpr,
                                workPerThreadN: ArithExpr, workPerThreadM: ArithExpr): Lambda = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), // Transposed
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
    Float,
    Float,
    (A, B, C, alpha, beta) => {
      // Undo the tiling
      Untile2D() o
        MapWrg(1)(fun( aRows =>
          MapWrg(0)(fun( bCols =>

            Map(Scatter(reorderStride(tileSizeM/workPerThreadM))) o Join() o
              Map(TransposeW() o Join() o Map(TransposeW())) o

              Join() o

              toGlobal(MapSeq(fun(x =>
                MapLcl(1)(fun(y =>
                  MapLcl(0)(fun( z =>
                    MapSeq(fun(a =>
                      MapSeq(fun(x =>
                        add(
                          mult(Get(x, 0), alpha),
                          mult(Get(x, 1),beta)
                        )
                      )) $ Zip(Get(a, 0), Get(a, 1))
                    )) $ Zip(Get(z, 0), Transpose() $ Get(z, 1))
                  )) $ Zip(Get(y, 0), Split(workPerThreadM) o ReorderStride(tileSizeM/workPerThreadM) o Transpose() $ Get(y, 1))
                )) $ Zip(x, Split(workPerThreadN) $ Get(bCols, 1))
              ))) o

              // Multiply all necessary combinations of tiles
              ReduceSeq(fun( (acc, pairOfTiles) =>

                fun(pairOfTiles =>

                  MapLcl(1)( fun(rowsA =>
                    MapLcl(0)( fun( colsB =>
                      Join() o ReduceSeq(fun((acc, rowElemPair) =>
                        MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) o
                          fun(rowElemPair =>
                            Zip(
                              Join() o toPrivate(MapSeq(MapSeq(
                                fun(aArray => MapSeq(fun(b =>
                                  mult.apply(aArray, b)
                                )) $ Get(rowElemPair, 1))) o toPrivate(MapSeq(id))
                              )) o Split(1) $ Get(rowElemPair, 0),
                              acc
                            )
                          ) o fun(rowElemPair =>
                          Tuple(
                            Get(rowElemPair, 0),
                            toPrivate(MapSeq(id)) $ Get(rowElemPair, 1)
                          )) $ rowElemPair
                      ), Get(colsB, 1)
                      ) $ Zip(Transpose() $ Get(rowsA, 0), Transpose() $ Get(colsB, 0))

                    )) $ Zip(Split(workPerThreadM) o ReorderStride(tileSizeM/workPerThreadM) o Transpose() $ Get(pairOfTiles, 1), Get(rowsA, 1))
                  ))  $ Zip(Split(workPerThreadN) o Transpose() $ Get(pairOfTiles, 0), acc)

                ) o

                  // Copy tiles to local memory
                  Unzip() o toLocal(MapLcl(1)(fun(pair =>
                  Unzip() o MapLcl(0)(fun( pair =>
                    Tuple(id $ Get(pair, 0), id $ Get(pair, 1))
                  )) $ Zip(Get(pair, 0), Get(pair, 1))
                ))) $ Zip(Get(pairOfTiles, 0), Get(pairOfTiles, 1))
              )
                , MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id)))) $ Value(0.0f,
                  ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeM/workPerThreadM), tileSizeN/workPerThreadN))
              ) $ Zip(Get(aRows, 0), Get(bCols, 0))

            // Tile the matrices
          )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeN) $ B, Get(aRows, 1))
        )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeM) $ A, Tile(tileSizeM, tileSizeN) $ C)
    })


  val clblas_kepler = {
    val tileSizeM = 8
    val tileSizeK = 4
    val tileSizeN = 8

    val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
      "{ return acc + (l * r); }",
      Seq(VectorType(Float, tileSizeN), Float, VectorType(Float, tileSizeN)),
      VectorType(Float, tileSizeN))

    val mult = UserFun("mult", Array("x", "y"), "{ return x * y; }",
      Seq(VectorType(Float, tileSizeN), VectorType(Float, tileSizeN)), VectorType(Float, tileSizeN))

    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      VectorType(Float, tileSizeN),
      VectorType(Float, tileSizeN),
      (A, B, C, alpha, beta) => {
        // Undo the tiling
        Untile2D() o
          MapGlb(1)(fun( aRows =>
            MapGlb(0)(fun( bCols =>

              toGlobal(fun(x =>
                MapSeq(fun(y =>
                  MapSeq(fun(z =>
                    VectorizeUserFun(tileSizeN, add)(
                      toPrivate(mult)(z._0, alpha),
                      toPrivate(mult)(z._1, beta)
                    )
                  )) $ Zip(y._0, asVector(tileSizeN) $ y._1)
                )) $ Zip(x, bCols._1)
              )) o
                Join() o

                // Multiply all necessary combinations of tiles
                ReduceSeq(fun( (acc, pairOfTiles) =>

                  fun(pairOfTiles =>
                    Map(Join()) o
                      MapSeq( fun(rowA =>
                        MapSeq( fun( colB =>
                          ReduceSeq(fun((acc, y) =>
                            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
                          ), Get(colB, 1)) $ Zip(Get(rowA, 0), Get(colB, 0))
                        )) $ Zip(Transpose() $ Get(pairOfTiles, 1), Get(rowA, 1))
                      )) $ Zip(Transpose() $ Get(pairOfTiles, 0), acc)
                  ) o

                    // Copy tiles to private memory
                    fun(pairOfTiles =>
                      Tuple(
                        toPrivate(MapSeq(
                          asScalar() o
                            MapSeq(VectorizeUserFun(tileSizeM, id)) o
                            asVector(tileSizeM)
                        )) $ Get(pairOfTiles, 0),

                        toPrivate(MapSeq(
                          MapSeq(VectorizeUserFun(tileSizeN, id)) o
                            asVector(tileSizeN)
                        )) $ Get(pairOfTiles, 1)
                      )) $ pairOfTiles
                )
                  , MapSeq(MapSeq(VectorizeUserFun(tileSizeN, id))) $
                    Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(VectorType(Float, tileSizeN), 1), tileSizeM))
                ) $ Zip(aRows._0, bCols._0)

            )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeN) $ B, aRows._1)
            // Tile the matrices
          )) $ Zip(
          Transpose() o Tile(tileSizeK, tileSizeM) $ A,
          Tile(tileSizeN, tileSizeM) $ C
        )
      })
  }

  val clblast_kepler = {

    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)
      val v__8 = variables(8)

      val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
      val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
      val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
      val mult2 = UserFun("mult2", Array("l", "r"), """|{ return l * r; }""".stripMargin,
        Seq(VectorType(Float, 2), VectorType(Float, 2)), VectorType(Float, 2))
      fun(
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_M_0),
        VectorType(Float, 2), VectorType(Float, 2),
        (p_0, p_1, C, alpha, beta) =>
          FunCall(Join(),
            FunCall(MapWrg(1)(fun((p_2) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(MapWrg(0)(fun((p_3) =>
                    FunCall(TransposeW(),
                      FunCall(Join(),
                        FunCall(Map(fun((p_4) =>
                          Map(Join() o Scatter(ReorderWithStride(v__5/v__3 * 2)) o Split(2)) $
                            FunCall(TransposeW(),
                              FunCall(Join(),
                                FunCall(Map(fun((p_5) =>
                                  FunCall(TransposeW(),
                                    FunCall(Map(fun((p_6) =>
                                      FunCall(TransposeW(), p_6))),
                                      FunCall(TransposeW(), p_5))))),
                                  FunCall(TransposeW(), p_4)))))),
                          FunCall(TransposeW(),
                            FunCall(MapSeq(fun((p_7) =>
                              FunCall(toGlobal(fun((p_8) =>
                                FunCall(MapLcl(1)(fun((p_9) =>
                                  FunCall(MapLcl(0)(fun((p_10) =>
                                    FunCall(MapSeq(fun((p_11) =>
                                      FunCall(asScalar(),
                                        FunCall(MapSeq(fun((p_12) =>
                                          FunCall(VectorizeUserFun(2,add),
                                            toPrivate(mult2)(p_12._0, alpha),
                                            toPrivate(mult2)(p_12._1, beta)
                                          ))),
                                          Zip(FunCall(asVector(2), p_11._0), asVector(2) $ p_11._1))))),
                                      Zip(p_10._0, Transpose() $ p_10._1)))),
                                    Zip(p_9._0, Split(v__3) o Join() o Gather(ReorderWithStride(v__5/v__3* 2)) o Split(2) o Transpose() $ p_9._1)))),
                                  Zip(p_8, Split(v__4) $ p_3._1)
                                ))), p_7))),
                              FunCall(ReduceSeq(fun((p_13, p_14) =>
                                FunCall(fun((p_15) =>
                                  FunCall(MapLcl(1)(fun((p_16) =>
                                    FunCall(Join(),
                                      FunCall(MapLcl(0)(fun((p_17) =>
                                        FunCall(MapSeq(fun((p_18) => p_18)),
                                          FunCall(ReduceSeq(fun((p_19, p_20) =>
                                            FunCall(fun((p_21) =>
                                              FunCall(MapSeq(fun((p_22) =>
                                                FunCall(MapSeq(fun((p_23) =>
                                                  FunCall(add,
                                                    FunCall(Get(0), p_23),
                                                    FunCall(mult,
                                                      FunCall(Get(1), p_22),
                                                      FunCall(Get(1), p_23))))),
                                                  FunCall(Zip(2),
                                                    FunCall(Get(0), p_22),
                                                    FunCall(Get(1), p_21))))),
                                                FunCall(Zip(2), p_19,
                                                  FunCall(Get(0), p_21)))),
                                              FunCall(toPrivate(fun((p_24) =>
                                                FunCall(fun((p_25) =>
                                                  FunCall(Tuple(2),
                                                    FunCall(MapSeq(fun((p_26) =>
                                                      FunCall(idfloat, p_26))),
                                                      FunCall(Get(0), p_25)),
                                                    FunCall(MapSeq(fun((p_27) =>
                                                      FunCall(idfloat, p_27))),
                                                      FunCall(Get(1), p_25)))), p_24))), p_20)))),
                                            FunCall(Get(0), p_17),
                                            FunCall(Zip(2),
                                              FunCall(Transpose(),
                                                FunCall(Get(1), p_16)),
                                              FunCall(Transpose(),
                                                FunCall(Get(1), p_17))))))),
                                        FunCall(Zip(2),
                                          FunCall(Get(0), p_16),
                                          FunCall(Split(v__3),
                                            Join() o Gather(ReorderWithStride(v__5/v__3* 2)) o Split(2) $
                                              FunCall(Transpose(),
                                                FunCall(Get(1), p_15)))))))),
                                    FunCall(Zip(2), p_13,
                                      FunCall(Split(v__4),
                                        FunCall(Transpose(),
                                          FunCall(Get(0), p_15)))))),
                                  FunCall(toLocal(fun((p_28) =>
                                    FunCall(fun((p_29) =>
                                      FunCall(Tuple(2),
                                        FunCall(Split(v__5),
                                          FunCall(Join(),
                                            FunCall(MapLcl(1)(fun((p_30) =>
                                              FunCall(asScalar(),
                                                FunCall(MapLcl(0)(fun((p_31) =>
                                                  FunCall(VectorizeUserFun(2,idfloat), p_31))),
                                                  FunCall(asVector(2), p_30))))),
                                              FunCall(Split(v__6),
                                                FunCall(Join(),
                                                  FunCall(Get(0), p_29)))))),
                                        FunCall(MapLcl(1)(fun((p_32) =>
                                          FunCall(asScalar(),
                                            FunCall(MapLcl(0)(fun((p_33) =>
                                              FunCall(VectorizeUserFun(2,idfloat), p_33))),
                                              FunCall(asVector(2), p_32))))),
                                          FunCall(Get(1), p_29)))), p_28))), p_14)))),
                                FunCall(MapLcl(1)(fun((p_34) =>
                                  FunCall(MapLcl(0)(fun((p_35) =>
                                    FunCall(MapSeq(fun((p_36) =>
                                      FunCall(MapSeq(fun((p_37) =>
                                        FunCall(idfloat, p_37))), p_36))), p_35))), p_34))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4), v__7 * 1 /^ v__3), v__5 * 1 /^ v__4))),
                                FunCall(Zip(2), p_2._0, p_3._0))))))))),
                    Zip(FunCall(Transpose(),
                      FunCall(Map(fun((p_38) =>
                        FunCall(Transpose(), p_38))),
                        FunCall(Split(v__8),
                          FunCall(Map(fun((p_39) =>
                            FunCall(Split(v__7), p_39))), p_1)))),
                      p_2._1
                    )
                  ))))),
              Zip(
                FunCall(Transpose(),
                  FunCall(Map(fun((p_40) =>
                    FunCall(Transpose(), p_40))),
                    FunCall(Split(v__8),
                      FunCall(Map(fun((p_41) =>
                        FunCall(Split(v__5), p_41))), p_0)))),
                Tile(v__5, v__7) $ C
              ))
          ))
    }

    val param = 8

    factory(Seq[ArithExpr](M, K, N,param,8,64, 128, 128, 16))
  }

  val clblast_hawaii = {

    val v__3 = 4
    val v__4 = 8
    val vectorWidth = 4

    // TODO: the store isn't actually vectorised in the source
    val mult2 = UserFun("mult2", Array("x", "y"), "{ return x * y; }",
      Seq(VectorType(Float, vectorWidth), VectorType(Float, vectorWidth)), VectorType(Float, vectorWidth))
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      VectorType(Float, vectorWidth),VectorType(Float, vectorWidth),
      (p_0, p_1, C, alpha, beta) =>
        FunCall(Map(fun((p_2) =>
          p_2)),
          FunCall(Join(),
            FunCall(MapGlb(1)(fun((p_3) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(MapGlb(0)(fun((p_4) =>
                    FunCall(TransposeW(),
                      FunCall(Map(fun((p_5) =>
                        FunCall(TransposeW(), p_5))),
                        FunCall(TransposeW(),
                          toGlobal(
                            MapSeq(fun(x =>
                              MapSeq(fun(z =>
                                asScalar() o MapSeq(fun(y =>
                                  VectorizeUserFun(4, add)(
                                    toPrivate(mult2)(y._0, alpha),
                                    toPrivate(mult2)(y._1, beta)
                                  )
                                )) $ Zip(asVector(4) $ z._0, asVector(4) $ z._1)
                              )) $ Zip(x, p_4._1)
                            ))
                          ) $
                            FunCall(ReduceSeq(fun((p_6, p_7) =>
                              fun(x =>
                                FunCall(MapSeq(fun((p_8) =>
                                  FunCall(MapSeq(fun((p_9) =>
                                    FunCall(add,
                                      FunCall(Get(0), p_9),
                                      FunCall(mult,
                                        FunCall(Get(1), p_8),
                                        FunCall(Get(1), p_9))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_8),
                                      FunCall(Get(1), x))))),
                                  FunCall(Zip(2), p_6,
                                    FunCall(Get(0), x)))
                              ) $ Tuple(
                                toPrivate(MapSeq(id)) o Get(0) $ p_7,
                                asScalar() o toPrivate(MapSeq(VectorizeUserFun(4, id))) o asVector(vectorWidth) o Get(1) $ p_7
                              ))),
                              toPrivate(MapSeq(MapSeq(id))) $
                                Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4)),
                              FunCall(Zip(2),
                                FunCall(Transpose(), p_3._0),
                                FunCall(Transpose(), p_4._0)))))))),
                    Zip(FunCall(Split(v__3),
                      /*FunCall(Gather(ReorderWithStride(N / v__3)), */Transpose() $ p_1),
                    p_3._1))
                )))),
              Zip(FunCall(Split(v__4), Transpose() $ p_0), Tile(v__4, v__3) $ C))
          )))
  }

  val clblas_hawaii = {
    val tileSizeM = 4
    val tileSizeK = 4
    val tileSizeN = 8

    // TODO: Use mad instead of multAndSumUp?
    val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
      "{ return acc + (l * r); }",
      Seq(VectorType(Float, tileSizeN), Float, VectorType(Float, tileSizeN)),
      VectorType(Float, tileSizeN))

    val mult = UserFun("mult", Array("x", "y"), "{ return x * y; }",
      Seq(VectorType(Float, tileSizeN), VectorType(Float, tileSizeN)), VectorType(Float, tileSizeN))

    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      VectorType(Float, tileSizeN),
      VectorType(Float, tileSizeN),
      (A, B, C, alpha, beta) => {
        // Undo the tiling
        Untile2D() o
          MapGlb(1)(fun( aRows =>
            MapGlb(0)(fun( bCols =>

              toGlobal(fun(z =>
                MapSeq(fun(x =>
                  MapSeq(fun(y =>
                    VectorizeUserFun(tileSizeN, add)(
                      toPrivate(mult)(y._0, alpha),
                      toPrivate(mult)(y._1, beta)
                    )
                  )) $ Zip(x._0, asVector(tileSizeN) $ x._1)
                )) $ Zip(z, bCols._1)
              )) o
                Join() o

                // Multiply all necessary combinations of tiles
                ReduceSeq(fun( (acc, pairOfTiles) =>

                  fun(pairOfTiles =>
                    Map(Join()) o
                      MapSeq( fun(rowA =>
                        MapSeq( fun( colB =>
                          ReduceSeq(fun((acc, y) =>
                            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
                          ), Get(colB, 1)) $ Zip(Get(rowA, 0), Get(colB, 0))
                        )) $ Zip(Transpose() $ Get(pairOfTiles, 1), Get(rowA, 1))
                      )) $ Zip(Transpose() $ Get(pairOfTiles, 0), acc)
                  ) o

                    // Copy tiles to private memory
                    fun(pairOfTiles =>
                      Tuple(
                        toPrivate(MapSeq(
                          asScalar() o
                            MapSeq(VectorizeUserFun(tileSizeK, id)) o
                            asVector(tileSizeK)
                        )) $ Get(pairOfTiles, 0),

                        toPrivate(MapSeq(
                          MapSeq(VectorizeUserFun(tileSizeN, id)) o
                            asVector(tileSizeN)
                        )) $ Get(pairOfTiles, 1)
                      )) $ pairOfTiles
                )
                  , MapSeq(MapSeq(VectorizeUserFun(tileSizeN, id))) $
                    Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(VectorType(Float, tileSizeN), 1), tileSizeM))
                ) $ Zip(aRows._0, bCols._0)

            )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeN) $ B, aRows._1)
            // Tile the matrices
          )) $ Zip(
          Transpose() o Tile(tileSizeK, tileSizeM) $ A,
          Tile(tileSizeM, tileSizeN) $ C
        )
      })
  }


  def apply() = new GEMM(
    Seq(("naive", Array[Lambda](naive)),
      ("tiledAndBlockedBInnermost", Array[Lambda](tiledAndBlockedBInnermost(16, 16, 8, 4, 4))),
      ("clblast_kepler", Array[Lambda](clblast_kepler)),
      ("clblast_hawaii", Array[Lambda](clblast_hawaii)),
      ("clblas_kepler", Array[Lambda](clblas_kepler)),
      ("clblas_hawaii", Array[Lambda](clblas_hawaii))

    ))


  def main(args: Array[String]): Unit = {
    val gemm = GEMM() 
    if (!gemm.gemmParser.parse(args))
      throw new IllegalArgumentException("Wrong command line arguments passed")
    
    gemm.run(args)
  }
}
