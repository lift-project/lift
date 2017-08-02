package exploration

import com.typesafe.scalalogging.Logger
import lift.arithmetic.{ArithExpr, Cst}
import org.clapper.argot.{FlagOption, SingleValueOption}
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class GenericKernelPrinterSettings (
  speedup: Float,
  configsToAchieveSpeedup: Int,
  timeoutInSeconds: Int,
  maxConfigs: Int,
  maxLocalSize: Int,
  vendor: String,
  deviceType: String,
  deviceId: Int
) {
  override def toString: String =
  s"""GenericKernelPrinterSettings:
     |    speedup: $speedup
     |    configsToAchieveSpeedup: $configsToAchieveSpeedup
     |    timeoutInSeconds: $timeoutInSeconds
     |    maxConfigs: $maxConfigs
     |    maxLocalSize: $maxLocalSize
     |    vendor: $vendor
     |    deviceType: $deviceType
     |    deviceId: $deviceId
   """.stripMargin
}

object GenericKernelPrinterSettings {

  import GenericKernelPrinter._
  import exploration.utils.ExplorationParameter._

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None)
  def createWithDefaults(
                        configSpeedup: Option[Float],
                        configConfigsToAchieveSpeedup: Option[Int],
                        configTimeoutInSeconds: Option[Int],
                        configMaxConfigs: Option[Int],
                        configMaxLocalSize: Option[Int],
                        configVendor: Option[String],
                        configDeviceType: Option[String],
                        configDeviceId: Option[Int]
                        ) = GenericKernelPrinterSettings (
    getValue(speedup, configSpeedup, defaultSpeedup),
    getValue(configsToAchieveSpeedup, configConfigsToAchieveSpeedup, defaultConfigsToAchieveSpeedup),
    getValue(timeoutInSeconds, configTimeoutInSeconds, defaultTimeoutInSeconds),
    getValue(maxConfigs, configMaxConfigs, defaultMaxConfigs),
    getValue(maxLocalSize, configMaxLocalSize, defaultMaxLocalSize),
    getValue(vendor, configVendor, defaultVendor),
    getValue(deviceType, configDeviceType, defaultDeviceType),
    getValue(deviceId, configDeviceId, defaultDeviceId))
}

case class LocalMemoryRulesSettings (
  addIdForCurrentValueInReduce: Boolean,
  addIdMapLcl: Boolean,
  addIdMapWrg: Boolean,
  addIdAfterReduce: Boolean
) {
  override def toString: String =
     s"""LocalMemoryRulesSettings:
        |    addIdForCurrentValueInReduce: $addIdForCurrentValueInReduce
        |    addIdMapLcl: $addIdMapLcl
        |    addIdMapWrg: $addIdMapWrg
        |    addIdAfterReduce: $addIdAfterReduce
      """.stripMargin
}

object LocalMemoryRulesSettings{

  import MemoryMappingRewrite._
  import utils.ExplorationParameter._

  def createDefault = createWithDefaults(None, None, None, None)
  def createWithDefaults(
                        configAddIdForCurrentValueInReduce: Option[Boolean],
                        configAddIdMapLcl: Option[Boolean],
                        configAddIdMapWrg: Option[Boolean],
                        configAddIdAfterReduce: Option[Boolean]
                        ) = LocalMemoryRulesSettings(
    getValue(addIdForCurrentValueInReduce, configAddIdAfterReduce, defaultAddIdForCurrentValueInReduce),
    getValue(addIdMapLcl, configAddIdMapLcl, defaultAddIdMapLcl),
    getValue(addIdMapWrg, configAddIdMapWrg, defaultAddIdMapWrg),
    getValue(addIdAfterReduce, configAddIdAfterReduce, defaultAddIdAfterReduce))
}

case class ParameterRewriteSettings(
  exploreNDRange: Boolean,
  sampleNDRange: Int,
  disableNDRangeInjection: Boolean,
  sequential: Boolean,
  generateScala: Boolean
) {
  override def toString: String =
  s"""ParameterRewriteSettings:
     |    exploreNDRange: $exploreNDRange
     |    sampleNDRange: $sampleNDRange
     |    disableNDRangeInjection: $disableNDRangeInjection
     |    sequential: $sequential
     |    generateScala: $generateScala
   """.stripMargin
}

object ParameterRewriteSettings {

  import ParameterRewrite._
  import exploration.utils.ExplorationParameter._

  def createDefault = createWithDefaults(None, None, None, None, None)
  def createWithDefaults(
                        configExploreNDRange: Option[Boolean],
                        configSampleNDRange: Option[Int],
                        configDisableNDRangeInjection: Option[Boolean],
                        configSequential: Option[Boolean],
                        configGenerateScala: Option[Boolean]
                        ) = ParameterRewriteSettings(
    getValue(exploreNDRange, configExploreNDRange, defaultExploreNDRange),
    getValue(sampleNDRange, configSampleNDRange, defaultSampleNDRange),
    getValue(disableNDRangeInjection, configDisableNDRangeInjection, defaultDisableNDRangeInjection),
    getValue(sequential, configSequential, defaultSequential),
    getValue(generateScala, configGenerateScala, defaultGenerateScala))
}

case class MemoryMappingRewriteSettings(
                                       vectorize: Boolean,
                                       vectorWidth: Int,
                                       sequential: Boolean,
                                       loadBalancing: Boolean,
                                       unrollReduce: Boolean,
                                       global0: Boolean,
                                       global01: Boolean,
                                       global10: Boolean,
                                       global012: Boolean,
                                       global210: Boolean,
                                       group0: Boolean,
                                       group01: Boolean,
                                       group10: Boolean
) {
  override def toString: String =
  s"""MemoryMappingRewriteSettings:
     |    vectorize: $vectorize
     |    vectorWidth: $vectorWidth
     |    sequential: $sequential
     |    loadBalancing: $loadBalancing
     |    unrollReduce: $unrollReduce
     |    global0: $global0
     |    global01: $global01
     |    global10: $global10
     |    global012: $global012
     |    global210: $global210
     |    group0: $group0
     |    group01: $group01
     |    group10: $group10
   """.stripMargin
}

object MemoryMappingRewriteSettings {

  import MemoryMappingRewrite._
  import exploration.utils.ExplorationParameter._

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None, None, None, None, None, None)
  def createWithDefaults(
                        configVectorize: Option[Boolean],
                        configVectorWidth: Option[Int],
                        configSequential: Option[Boolean],
                        configLoadBalancing: Option[Boolean],
                        configUnrollReduce: Option[Boolean],
                        configGlobal0: Option[Boolean],
                        configGlobal01: Option[Boolean],
                        configGlobal10: Option[Boolean],
                        configGlobal012: Option[Boolean],
                        configGlobal210: Option[Boolean],
                        configGroup0: Option[Boolean],
                        configGroup01: Option[Boolean],
                        configGroup10: Option[Boolean]
                        ) = MemoryMappingRewriteSettings(
  getValue(vectorize, configVectorize, defaultVectorize),
  getValue(vectorWidth, configVectorWidth, defaultVectorWidth),
  getValue(sequential, configSequential, defaultSequential),
  getValue(loadBalancing, configLoadBalancing, defaultLoadBalancing),
  getValue(unrollReduce, configUnrollReduce, defaultUnrollReduce),
  getValue(global0, configGlobal0, defaultGlobal0),
  getValue(global01, configGlobal01, defaultGlobal01),
  getValue(global10, configGlobal10, defaultGlobal10),
  getValue(global012, configGlobal012, defaultGlobal012),
  getValue(global210, configGlobal210, defaultGlobal210),
  getValue(group0, configGroup0, defaultGroup0),
  getValue(group01, configGroup01, defaultGroup01),
  getValue(group10, configGroup10, defaultGroup10))
}

case class HighLevelRewriteSettings(
  explorationDepth: Int,
  depth: Int,
  distance: Int,
  ruleRepetition: Int,
  vectorWidth: Int,
  sequential: Boolean,
  onlyLower: Boolean,
  oldStringRepresentation: Boolean,
  ruleCollection: String
) {
  override def toString: String =
    s"""HighLevelRewriteSettings:
      |    explorationDepth: $explorationDepth
      |    depth: $depth
      |    distance: $distance
      |    ruleRepetition: $ruleRepetition
      |    vectorWidth: $vectorWidth
      |    sequential: $sequential
      |    onlyLower: $onlyLower
      |    oldStringRepresentation: $oldStringRepresentation
      |    ruleCollection: $ruleCollection
    """.stripMargin
}

object HighLevelRewriteSettings {

  import HighLevelRewrite._
  import exploration.utils.ExplorationParameter._

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None, None)

  def createWithDefaults(
                        configExplorationDepth: Option[Int],
                        configDepth: Option[Int],
                        configDistance: Option[Int],
                        configRuleRepetition: Option[Int],
                        configVectorWidth: Option[Int],
                        configSequential: Option[Boolean],
                        configOnlyLower: Option[Boolean],
                        configOldStringRepresentation: Option[Boolean],
                        configRuleCollection: Option[String]
                        ) = HighLevelRewriteSettings(
  // priority: 1) command-line args; 2) config-file; 3) default values
  getValue(explorationDepth, configExplorationDepth, defaultExplorationDepth),
  getValue(depthFilter, configDepth, defaultDepthFilter),
  getValue(distanceFilter, configDistance, defaultDistanceFilter),
  getValue(ruleRepetition, configRuleRepetition, defaultRuleRepetition),
  getValue(vectorWidth, configVectorWidth, defaultVectorWidth),
  getValue(sequential, configSequential, defaultSequential),
  getValue(onlyLower, configOnlyLower, defaultOnlyLower),
  getValue(oldStringRepresentation, configOldStringRepresentation, defaultOldStringRepresentation),
  getValue(ruleCollection, configRuleCollection, defaultRuleCollection))
}

object SearchParameters {

  import ExpressionFilter._

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None)

  def createWithDefaults(
                          defaultInputSize: Option[Int],
                          minLocalSize: Option[Int],
                          maxLocalSize: Option[Int],
                          minGlobalSize: Option[Int],
                          maxPrivateMemory: Option[Int],
                          maxLocalMemory: Option[Int],
                          minWorkgroups: Option[Int],
                          maxWorkgroups: Option[Int]
  ) = SearchParameters(
    defaultInputSize.getOrElse(default_input_size),
    minLocalSize.getOrElse(min_local_size),
    maxLocalSize.getOrElse(max_local_size),
    minGlobalSize.getOrElse(min_global_size),
    maxPrivateMemory.getOrElse(max_private_memory),
    maxLocalMemory.getOrElse(max_local_memory),
    minWorkgroups.getOrElse(min_workgroups),
    maxWorkgroups.getOrElse(max_workgroups)
  )

}

case class SearchParameters(
                             defaultInputSize: Int,
                             minLocalSize: Int,
                             maxLocalSize: Int,
                             minGlobalSize: Int,
                             maxPrivateMemory: Int,
                             maxLocalMemory: Int,
                             minWorkgroups: Int,
                             maxWorkgroups: Int
)

case class Settings(
  inputCombinations: Option[Seq[Seq[ArithExpr]]] = None,
  searchParameters: SearchParameters = SearchParameters.createDefault,
  highLevelRewriteSettings: HighLevelRewriteSettings = HighLevelRewriteSettings.createDefault,
  memoryMappingRewriteSettings: MemoryMappingRewriteSettings = MemoryMappingRewriteSettings.createDefault,
  parameterRewriteSettings: ParameterRewriteSettings = ParameterRewriteSettings.createDefault,
  genericKernelPrinterSettings: GenericKernelPrinterSettings = GenericKernelPrinterSettings.createDefault,
  localMemoryRulesSettings: LocalMemoryRulesSettings= LocalMemoryRulesSettings.createDefault
) {

  override def toString: String = {
    s"""Settings(
       |  $inputCombinations,
       |  $searchParameters
       |  $highLevelRewriteSettings
       |  $memoryMappingRewriteSettings
       |  $parameterRewriteSettings
       |  $genericKernelPrinterSettings
       |  $localMemoryRulesSettings
       |)""".stripMargin
  }

}

object ParseSettings {

  private val logger = Logger(this.getClass)

  private[exploration] implicit val arithExprReads: Reads[ArithExpr] =
    JsPath.read[Long].map(Cst)

  private[exploration] implicit val parametersReads: Reads[SearchParameters] = (
    (JsPath \ "default_input_size").readNullable[Int] and
    (JsPath \ "min_local_size").readNullable[Int] and
    (JsPath \ "max_local_size").readNullable[Int] and
    (JsPath \ "min_global_size").readNullable[Int] and
    (JsPath \ "max_private_memory").readNullable[Int] and
    (JsPath \ "max_local_memory").readNullable[Int] and
    (JsPath \ "min_workgroups").readNullable[Int] and
    (JsPath \ "max_workgroups").readNullable[Int]
  )(SearchParameters.createWithDefaults _)

  private[exploration] implicit val highLevelReads: Reads[HighLevelRewriteSettings] = (
    (JsPath \ "exploration_depth").readNullable[Int] and
    (JsPath \ "depth").readNullable[Int] and
    (JsPath \ "distance").readNullable[Int] and
    (JsPath \ "rule_repetition").readNullable[Int] and
    (JsPath \ "vector_width").readNullable[Int] and
    (JsPath \ "sequential").readNullable[Boolean] and
    (JsPath \ "only_lower").readNullable[Boolean] and
    (JsPath \ "old_string_representation").readNullable[Boolean] and
    (JsPath \ "rule_collection").readNullable[String]
  )(HighLevelRewriteSettings.createWithDefaults _)

  private[exploration] implicit val memoryMappingReads: Reads[MemoryMappingRewriteSettings] = (
    (JsPath \ "vectorize").readNullable[Boolean] and
    (JsPath \ "vector_width").readNullable[Int] and
    (JsPath \ "sequential").readNullable[Boolean] and
    (JsPath \ "load_balancing").readNullable[Boolean] and
    (JsPath \ "unroll_reduce").readNullable[Boolean] and
    (JsPath \ "global0").readNullable[Boolean] and
    (JsPath \ "global01").readNullable[Boolean] and
    (JsPath \ "global10").readNullable[Boolean] and
    (JsPath \ "global012").readNullable[Boolean] and
    (JsPath \ "global210").readNullable[Boolean] and
    (JsPath \ "group0").readNullable[Boolean] and
    (JsPath \ "group01").readNullable[Boolean] and
    (JsPath \ "group10").readNullable[Boolean]
  )(MemoryMappingRewriteSettings.createWithDefaults _)

  private[exploration] implicit val parameterRewriteReads: Reads[ParameterRewriteSettings] = (
    (JsPath \ "explore_ndrange").readNullable[Boolean] and
    (JsPath \ "sample_ndrange").readNullable[Int] and
    (JsPath \ "disable_ndrange_injection").readNullable[Boolean] and
    (JsPath \ "sequential").readNullable[Boolean] and
    (JsPath \ "generate_scala").readNullable[Boolean]
  )(ParameterRewriteSettings.createWithDefaults _)

  private[exploration] implicit val genericReads: Reads[GenericKernelPrinterSettings] = (
    (JsPath \ "speedup").readNullable[Float] and
    (JsPath \ "configs_to_achieve_speedup").readNullable[Int] and
    (JsPath \ "timeout_in_seconds").readNullable[Int] and
    (JsPath \ "max_configs").readNullable[Int] and
    (JsPath \ "max_local_size").readNullable[Int] and
    (JsPath \ "vendor").readNullable[String] and
    (JsPath \ "device_type").readNullable[String] and
    (JsPath \ "device_id").readNullable[Int]
  )(GenericKernelPrinterSettings.createWithDefaults _)

  private[exploration] implicit val localMemoryRulesReads: Reads[LocalMemoryRulesSettings] = (
    (JsPath \ "addIdForCurrentValueInReduce").readNullable[Boolean] and
    (JsPath \ "addIdMapLcl").readNullable[Boolean] and
    (JsPath \ "addIdMapWrg").readNullable[Boolean] and
    (JsPath \ "addIdAfterReduce").readNullable[Boolean]
  )(LocalMemoryRulesSettings.createWithDefaults _)

  private[exploration] implicit val settingsReads: Reads[Settings] = (
    (JsPath \ "input_combinations").readNullable[Seq[Seq[ArithExpr]]] and
    (JsPath \ "search_parameters").readNullable[SearchParameters] and
    (JsPath \ "high_level_rewrite").readNullable[HighLevelRewriteSettings] and
    (JsPath \ "memory_mapping_rewrite").readNullable[MemoryMappingRewriteSettings] and
    (JsPath \ "parameter_rewrite").readNullable[ParameterRewriteSettings] and
    (JsPath \ "generic_kernel_printer").readNullable[GenericKernelPrinterSettings] and
    (JsPath \ "local_memory_rules").readNullable[LocalMemoryRulesSettings]
  )((maybeCombinations, maybeParameters, maybeHighLevel, maybeMemoryMapping, maybeParameterRewrite, maybeGeneric, maybeLocalMemoryRules) =>
    Settings(
      maybeCombinations,
      maybeParameters.getOrElse(SearchParameters.createDefault),
      maybeHighLevel.getOrElse(HighLevelRewriteSettings.createDefault),
      maybeMemoryMapping.getOrElse(MemoryMappingRewriteSettings.createDefault),
      maybeParameterRewrite.getOrElse(ParameterRewriteSettings.createDefault),
      maybeGeneric.getOrElse(GenericKernelPrinterSettings.createDefault),
      maybeLocalMemoryRules.getOrElse(LocalMemoryRulesSettings.createDefault)
    ))

  def apply(optionFilename: Option[String]): Settings =
    optionFilename match {
      case Some(filename) =>

        val settingsString = ParameterRewrite.readFromFile(filename)
        val json = Json.parse(settingsString)
        val validated = json.validate[Settings]

        validated match {
          case JsSuccess(settings, _) =>
            checkSettings(settings)
            settings
          case e: JsError =>
            logger.error("Failed parsing settings " +
              e.recoverTotal( e => JsError.toFlatJson(e) ))
            sys.exit(1)
        }

      case None => Settings()
    }

  private def checkSettings(settings: Settings): Unit = {

    // Check all combinations are the same size
    if (settings.inputCombinations.isDefined) {
      val sizes = settings.inputCombinations.get

      if (sizes.map(_.length).distinct.length != 1) {
        logger.error("Sizes read from settings contain different numbers of parameters")
        sys.exit(1)
      }
    }
  }

}
