package benchmarks.conv

import org.apache.commons.cli.{CommandLine, DefaultParser, OptionGroup, Options, ParseException}

object ConvExplorationCmdArgs {
  // The options that have to be specified as -D[option_name]=[option_value] arguments to JVM
  // A benefit of using these is that the logback can access them
  val requiredJVMOptions: List[String] = List(
    "experiment_label", "run_label")

  requiredJVMOptions.foreach(jvmOption => if (System.getProperty(jvmOption).equals("null"))
    throw new IllegalArgumentException(s"${jvmOption} missing. It has to be passed to JVM as " +
      s"'-D${jvmOption}=<value>'."))

  val options = new Options()

  // Example command line arguments:
  //--path_to_settings C:\Users\Naums\lift-rewrite-conv\src\test\xxx\xxx\settings\compileOnWindowsRunOnBoard.json
  // --net_name vgg --compile_for_board --overwrite_compiled_files --compilation_timeout 60 --total_tune_points 5
  // --tune_points_batch_size 1 --from_layer 2 --to_layer 3 --lambda_dir_label setting_up

  options.addOption(org.apache.commons.cli.Option.builder("path_to_settings").
    longOpt("path_to_settings").argName("path_to_settings").
    desc("Full path to the JSON file with the environment settings").
    `type`(classOf[String]).
    hasArg().
    required(true).
    build())

  val runModeOptions = new OptionGroup()
  runModeOptions.setRequired(true)
  runModeOptions.addOption(org.apache.commons.cli.Option.builder("compile_for_board").
    longOpt("compile_for_board").argName("compile_for_board").
    desc("Run mode: compile for the board, i.e. generate kernel(s) and host code " +
      "as well as bash scripts to run them on the board.").
    `type`(classOf[Boolean]).
    build())

  runModeOptions.addOption(org.apache.commons.cli.Option.builder("compile_and_run_on_desktop").
    longOpt("compile_and_run_on_desktop").argName("compile_and_run_on_desktop").
    desc("Run mode: compile and run on desktop, i.e. compile and run (+verify func correctness) on the same machine").
    `type`(classOf[Boolean]).
    build())

  options.addOptionGroup(runModeOptions)

  val testHarnessVersions = new OptionGroup()
  testHarnessVersions.setRequired(true)
  testHarnessVersions.addOption(org.apache.commons.cli.Option.builder("old_advanced_test_harness").
    longOpt("old_advanced_test_harness").argName("old_advanced_test_harness").
    desc("Use the old advanced test harness that reads layer configs from C++ files, compares golden " +
      "results against Caffe2-generated ones and does some additional timings. Implemented for CASES 2019").
    `type`(classOf[Boolean]).
    build())
  testHarnessVersions.addOption(org.apache.commons.cli.Option.builder("new_simpler_test_harness").
    longOpt("new_simpler_test_harness").argName("new_simpler_test_harness").
    desc("The new simpler test harness developed for HiPEAC 2020. To be deprecated in favour of the old one.").
    `type`(classOf[Boolean]).
    build())

  options.addOptionGroup(testHarnessVersions)

  options.addOption(org.apache.commons.cli.Option.builder("net_name").
    longOpt("net_name").argName("net_name").
    desc("Network to run. See layer_configs package for accepted network architectures.").
    `type`(classOf[String]).
    hasArg().
    required(true).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("from_layer").
    longOpt("from_layer").argName("from_layer").
    desc("The first layer to explore (optional)").
    `type`(classOf[Int]).
    hasArg().
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("to_layer").
    longOpt("to_layer").argName("to_layer").
    desc("The last layer to explore (optional)").
    `type`(classOf[Int]).
    hasArg().
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("total_tune_points").
    longOpt("total_tune_points").argName("total_tune_points").
    desc("Total number of tuning points to explore").
    `type`(classOf[Int]).
    hasArg().
    required(true).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("tune_points_batch_size").
    longOpt("tune_points_batch_size").argName("tune_points_batch_size").
    desc("The size of the tuning batch, i.e. how many points to explore in each layer before switching to " +
      "the next layer. When a batch (i) is explored for all layers, batch (i+1) is explored. This allows to explore " +
      "all layers in parallel.").
    `type`(classOf[Int]).
    hasArg().
    required(true).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("restore_solver_states").
    longOpt("restore_solver_states").argName("restore_solver_states").
    desc("Restore constraint solver states from previous runs instead of initializing the new ones. " +
      "The back up path is to be set in JSON settings.").
    `type`(classOf[Boolean]).
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("back_up_solver_states").
    longOpt("back_up_solver_states").argName("back_up_solver_states").
    desc("Back up constraint solver states after each solution discovery. " +
      "The back up path is to be set in JSON settings.").
    `type`(classOf[Boolean]).
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("continue_from_layer").
    longOpt("continue_from_layer").argName("continue_from_layer").
    desc("The layer to start exploring from").
    `type`(classOf[Int]).
    hasArg().
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("continue_from_tune_point").
    longOpt("continue_from_tune_point").argName("continue_from_tune_point").
    desc("The tuning point to restart exploring from using restored solver states in layer <continue_from_point>").
    `type`(classOf[Int]).
    hasArg().
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("overwrite_compiled_files").
    longOpt("overwrite_compiled_files").argName("overwrite_compiled_files").
    desc("Overwrite kernels, host code and bash scripts generated in the previous runs or exit when " +
      "previous compilations are found").
    `type`(classOf[Boolean]).
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("compilation_timeout").
    longOpt("compilation_timeout").argName("compilation_timeout").
    desc("Compilation timeout (in seconds): allow a certain period of time for Lift to compile the kernel " +
      "and host code before the compilation thread is killed and the benchmark switches to the next point. " +
      "Even if the compilation is killed, the tuning point folder is still created with incomplete set of files. " +
      "This reduces the amount of time the benchmark will spend on kernels whose OpenCL code is too long and is unlikely " +
      "to yield good performance.").
    `type`(classOf[Int]).
    hasArg().
    required(true).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("enforce_solution_uniqueness_through_cstr").
    longOpt("enforce_solution_uniqueness_through_cstr").argName("enforce_solution_uniqueness_through_cstr").
    desc("Enforce that all solutions are unique within the scope of each layer by adding a new constraint " +
      "after discovering each new solution.").
    `type`(classOf[Boolean]).
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("log_git_status").
    longOpt("log_git_status").argName("log_git_status").
    desc("Log git status").
    `type`(classOf[Boolean]).
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("rewind_mode").
    longOpt("rewind_mode").argName("rewind_mode").
    desc("Reproduce param values of tune points that are skipped").
    `type`(classOf[Boolean]).
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("initial_seed").
    longOpt("initial_seed").argName("initial_seed").
    desc("Optional initial seed for the experiments. If given, overrides the one specified in the JSON file.").
    `type`(classOf[Int]).
    hasArg().
    build())

  options.addOption(org.apache.commons.cli.Option.builder("lambda_dir_subroot").
    longOpt("lambda_dir_subroot").argName("lambda_dir_subroot").
    desc("Subdirectory of jsonSettings.lambdaDirRoot where to save kernels and lambdas. If not given, " +
      "the files will be saved in lambdaDirRoot. Useful for eddie.").
    `type`(classOf[String]).
    hasArg().
    build())

//  options.addOption(org.apache.commons.cli.Option.builder("use_old_data_layout").
//    longOpt("use_old_data_layout").argName("use_old_data_layout").
//    desc("Use the old data layout (X=[H,W,IC],K=[OC,KH,KW,IC]) as opposed to the " +
//      "new one (X=[IC,H,W],K=[OC,IC,KH,KW])").
//    `type`(classOf[Boolean]).
//    build())

//  options.addOption(org.apache.commons.cli.Option.builder("do_depthwise_tiling_part_reduce_only").
//    longOpt("do_depthwise_tiling_part_reduce_only").argName("do_depthwise_tiling_part_reduce_only").
//    desc("Only do partial reduction, leaving final reduction for another kernel").
//    `type`(classOf[Boolean]).
//    build())

  options.addOption(org.apache.commons.cli.Option.builder("par_mappings_csv").
    longOpt("par_mappings_csv").argName("par_mappings_csv").
    desc("An optional path to a CSV file with parFuse mappings extracted from a generation log. During the " +
      "search, the system will iterate across these mappings instead of generating new ones").
    `type`(classOf[String]).
    hasArg().
    build())

  options.addOption(org.apache.commons.cli.Option.builder("job_id").
    longOpt("job_id").argName("job_id").
    desc("Job ID in a cluster").
    `type`(classOf[String]).
    hasArg().
    build())

  options.addOption(org.apache.commons.cli.Option.builder("skip_uncompilable_csv_par_mappings").
    longOpt("skip_uncompilable_csv_par_mappings").argName("skip_uncompilable_csv_par_mappings").
    desc("Skip par mappings from par_mappings_csv that cannot be compiled from the first try").
    `type`(classOf[Boolean]).
    required(false).
    build())


  options.addOption(org.apache.commons.cli.Option.builder("choose_local_size_deterministically").
    longOpt("choose_local_size_deterministically").argName("choose_local_size_deterministically").
    desc("Choose local sizes deterministically, using a Scala heuristic").
    `type`(classOf[Boolean]).
    required(false).
    build())


  options.addOption(org.apache.commons.cli.Option.builder("generate_par_mappings_without_solver").
    longOpt("generate_par_mappings_without_solver").argName("generate_par_mappings_without_solver").
    desc("Generate parallel mappings without the solver; use the solver only to verify mapping correctness.").
    `type`(classOf[Boolean]).
    required(false).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("first_search_space_to_explore").
    longOpt("first_search_space_to_explore").argName("first_search_space_to_explore").
    desc("The ID of the first search space to explore, i.e. pick one combination in each of the preceding " +
      "search spaces, and do the full exploration of the chosen space, and the subsequent spaces. For the space ID values, " +
      "see benchmarks.conv.LayerSearch.SearchSpace enum.").
    `type`(classOf[Int]).
    hasArg().
    required(true).
    build())

  options.addOption(org.apache.commons.cli.Option.builder("experiment_label").
    longOpt("experiment_label").argName("experiment_label").
    desc("Experiment label").
    `type`(classOf[String]).
    hasArg().
    required(false).
    build())


  def parseCommandLine(args: Array[String]): (CommandLine, Predef.Map[String, String]) = {
    try {
      val parser = new DefaultParser()
      (parser.parse(options, args),
        requiredJVMOptions.map(jvmOption => jvmOption -> System.getProperty(jvmOption)).toMap)
    } catch {
      case x: ParseException =>
        throw new ParseException("Option parsing failed. Reason: " + x.getMessage)
    }
  }
}
