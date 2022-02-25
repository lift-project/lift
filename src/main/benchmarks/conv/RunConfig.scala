package benchmarks.conv

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import org.apache.commons.cli.CommandLine
import benchmarks.conv.layer_configs.NetConfig
import benchmarks.conv.layer_configs.NetConfig
import utils.ShellUtils

case class RunConfig(runMode: RunConfig.Mode,
                     netConfig: NetConfig,
                     fromLayer: Int,
                     toLayer: Int,
                     totalTunePoints: Int,
                     tuneBatchSize: Int,
                     totalTuningBatches: Int,
                     overwriteCompiledFiles: Boolean,
                     compilationTimeout: Int,
                     benchmarkStartTime: Date,
                     experimentLabel: String,
                     enforceSolutionUniquenessThroughCstr: Boolean,
                     useCases2019TestHarness: Boolean,
                     restoreSolverStates: Boolean,
                     backUpSolverStates: Boolean,
                     continueFromLayer: Option[Int],
                     continueFromTunePointForTheContinueFromLayer: Option[Int],
                     rewindMode: Boolean,
                     initialSeed: Int,
                     lambdaDirSubroot: Option[String],
                     parMappingsCsv: Option[String],
                     jobId: Option[Int],
                     skipUncompilableCsvParMappings: Boolean,
                     chooseLocalSizeDeterministically: Boolean,
                     generateParMappingsWithoutSolver: Boolean,
                     firstSearchSpaceToExplore: Int) {
  lazy val partialBatchIdx: Option[Int] = continueFromTunePointForTheContinueFromLayer match {
    case Some(idx) => Some(idx / tuneBatchSize)
    case None => None
  }

  lazy val layersToExplore: List[Int] = (fromLayer to toLayer).filter(netConfig.getLayerIds.contains).toList

  override def toString: String =
    s"""runMode = $runMode
       |netConfig = $netConfig
       |fromLayer = $fromLayer ; toLayer = $toLayer
       |totalTunePoints = $totalTunePoints ; tuneBatchSize = $tuneBatchSize ; totalTuningBatches = $totalTuningBatches
       |overwriteCompiledFiles = $overwriteCompiledFiles
       |compilationTimeout = $compilationTimeout
       |experimentLabel = $experimentLabel
       |benchmarkStartTime = ${new SimpleDateFormat("dd.MM.yyyy HH:mm:ss").format(benchmarkStartTime)}
       |enforceSolutionUniqueness = $enforceSolutionUniquenessThroughCstr
       |useCases2019TestHarness = $useCases2019TestHarness
       |restoreSolverStates = $restoreSolverStates
       |backUpSolverStates = $backUpSolverStates
       |continueFromLayer = $continueFromLayer
       |continueFromTunePointForTheContinueFromLayer = $continueFromTunePointForTheContinueFromLayer
       |rewindMode = $rewindMode
       |initialSeed = $initialSeed
       |lambdaDirSubroot = $lambdaDirSubroot
       |parMappingsCsv = $parMappingsCsv
       |jobId = $jobId
       |skipUncompilableCsvParMappings = $skipUncompilableCsvParMappings
       |chooseLocalSizeDeterministically = $chooseLocalSizeDeterministically
       |generateParMappingsWithoutSolver = $generateParMappingsWithoutSolver
       |firstSearchSpaceToExplore = $firstSearchSpaceToExplore (${LayerSearch.SearchSpace.getEnum(firstSearchSpaceToExplore)})""".stripMargin

  def kernelDir(layerConfigIdx: Int, tunePointIdx: Int)(implicit jsonSettings: ConvExplorationJSONSettings): String = {
    val path = Seq(jsonSettings.lambdaDirRoot) ++ (
      if (lambdaDirSubroot.isDefined) Seq(lambdaDirSubroot.get) else Seq()) ++
      Seq("generated_files_" + experimentLabel,
        layerConfigIdx.toString, tunePointIdx.toString)
    ShellUtils.join(path: _*)
  }

  def lambdaDir(layerConfigIdx: Int, tunePointIdx: Int)(implicit jsonSettings: ConvExplorationJSONSettings): String = {
    val path = Seq(jsonSettings.lambdaDirRoot) ++ (
      if (lambdaDirSubroot.isDefined) Seq(lambdaDirSubroot.get) else Seq()) ++
      Seq("generated_lambdas_" + experimentLabel,
        layerConfigIdx.toString, tunePointIdx.toString)
    ShellUtils.join(path: _*)
  }
}

object RunConfig {

  object Mode extends Enumeration {
    type Mode = Value
    val compileForBoard: Mode = Value("compileForBoard")
    val compileAndRunOnDesktop: Mode = Value("compileAndRunOnDesktop")
  }
  type Mode = Mode.Mode


  def apply(cmdArgs: CommandLine,
            netConfig: NetConfig,
            jvmOptions: Predef.Map[String, String],
            jsonSettings: ConvExplorationJSONSettings): RunConfig = {

    val totalTunePoints = cmdArgs.getOptionValue("total_tune_points").toInt
    val tuneBatchSize = cmdArgs.getOptionValue("tune_points_batch_size").toInt

    if (cmdArgs.hasOption("restore_solver_states") &&
      !(cmdArgs.hasOption("continue_from_layer") &&
        cmdArgs.hasOption("continue_from_tune_point")))
      throw new IllegalArgumentException("When restore_solver_states is specified, both " +
        "continue_from_layer and continue_from_tune_point are compulsory")

    val (continueFromLayer, continueFromTunePoint) =
      if (cmdArgs.hasOption("restore_solver_states")) {

        (Some(cmdArgs.getOptionValue("continue_from_layer").toInt),
          Some(cmdArgs.getOptionValue("continue_from_tune_point").toInt))
      } else (None, None)

    new RunConfig(
      runMode = if (cmdArgs.hasOption("compile_for_board"))
        RunConfig.Mode.compileForBoard
      else
        RunConfig.Mode.compileAndRunOnDesktop,

      netConfig = netConfig,
      fromLayer = cmdArgs.getOptionValue("from_layer", "0").toInt,
      toLayer = cmdArgs.getOptionValue("to_layer", (netConfig.nLayers - 1).toString).toInt,

      totalTunePoints = totalTunePoints, tuneBatchSize = tuneBatchSize,
      totalTuningBatches = Math.max(1, Math.ceil(totalTunePoints.toFloat / tuneBatchSize).toInt),

      overwriteCompiledFiles = cmdArgs.hasOption("overwrite_compiled_files"),
      compilationTimeout = cmdArgs.getOptionValue("compilation_timeout").toInt,
        experimentLabel = if (jvmOptions.contains("experiment_label"))
          jvmOptions("experiment_label")
        else cmdArgs.getOptionValue("experiment_label"),
      benchmarkStartTime = Calendar.getInstance().getTime,
      enforceSolutionUniquenessThroughCstr = cmdArgs.hasOption("enforce_solution_uniqueness_through_cstr"),
      useCases2019TestHarness = cmdArgs.hasOption("old_advanced_test_harness"),
      restoreSolverStates = cmdArgs.hasOption("restore_solver_states"),
      backUpSolverStates = cmdArgs.hasOption("back_up_solver_states"),
      continueFromLayer = continueFromLayer,
      continueFromTunePointForTheContinueFromLayer = continueFromTunePoint,
      rewindMode = cmdArgs.hasOption("rewind_mode"),
      initialSeed =
        if (cmdArgs.hasOption("initial_seed")) cmdArgs.getOptionValue("initial_seed").toInt
        else jsonSettings.randomSeed,
      lambdaDirSubroot =
        if (cmdArgs.hasOption("lambda_dir_subroot")) Some(cmdArgs.getOptionValue("lambda_dir_subroot"))
        else None,
      parMappingsCsv =
        if (cmdArgs.hasOption("par_mappings_csv")) Some(cmdArgs.getOptionValue("par_mappings_csv"))
        else None,
      jobId =
        if (cmdArgs.hasOption("job_id")) Some(cmdArgs.getOptionValue("job_id").toInt)
        else None,
      skipUncompilableCsvParMappings = cmdArgs.hasOption("skip_uncompilable_csv_par_mappings"),
      chooseLocalSizeDeterministically = cmdArgs.hasOption("choose_local_size_deterministically"),
      generateParMappingsWithoutSolver = cmdArgs.hasOption("generate_par_mappings_without_solver"),
      firstSearchSpaceToExplore = cmdArgs.getOptionValue("first_search_space_to_explore").toInt)
  }
}