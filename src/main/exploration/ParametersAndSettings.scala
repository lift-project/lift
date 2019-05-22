package exploration

import com.typesafe.scalalogging.Logger
import exploration.utils.ExplorationParameter
import lift.arithmetic.{ArithExpr, Cst}
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.collection.immutable.ListMap
import scala.io.Source

abstract class ExplorationSettings {
  val defaultParameters : ListMap[String, Any]
  def generateConfigString : String
}

case class LocalMemoryRulesSettings(
                                     addIdForCurrentValueInReduce: Boolean,
                                     addIdMapLcl: Boolean,
                                     addIdMapWrg: Boolean,
                                     addIdAfterReduce: Boolean
                                   ) extends ExplorationSettings {
  override def toString: String =
    s"""${LocalMemoryRulesSettings.keyLocalMemoryRulesSettings}:
       |    ${LocalMemoryRulesSettings.defaultParameters.mkString("\n")}
      """.stripMargin

  override val defaultParameters: ListMap[String, Any] = LocalMemoryRulesSettings.defaultParameters
  override def generateConfigString : String = LocalMemoryRulesSettings.generateConfigString
}

object LocalMemoryRulesSettings {

  private[exploration] val defaultAddIdForCurrentValueInReduce = true
  private[exploration] val defaultAddIdMapLcl = true
  private[exploration] val defaultAddIdMapWrg = true
  private[exploration] val defaultAddIdAfterReduce = true

  private[exploration] val keyLocalMemoryRulesSettings = "local_memory_settings"
  private[exploration] val keyAddIdForCurrentValueInReduce = "add_id_for_current_value_in_reduce"
  private[exploration] val keyAddIdMapLcl = "add_id_maplcl"
  private[exploration] val keyAddIdMapWrg = "add_id_mapwrg"
  private[exploration] val keyAddIdAfterReduce = "add_id_after_reduce"

  private[exploration] val defaultParameters = ListMap(
    keyAddIdForCurrentValueInReduce -> defaultAddIdForCurrentValueInReduce,
    keyAddIdMapLcl -> defaultAddIdMapLcl,
    keyAddIdMapWrg -> defaultAddIdMapWrg,
    keyAddIdAfterReduce -> defaultAddIdAfterReduce
  )

  import MemoryMappingRewrite._
  import utils.ExplorationParameter._

  def generateConfigString : String = {
    ExplorationParameter.generateConfigString(keyLocalMemoryRulesSettings, defaultParameters)
  }

  def createDefault = createWithDefaults(None, None, None, None)

  def createWithDefaults(
                          configAddIdForCurrentValueInReduce: Option[Boolean],
                          configAddIdMapLcl: Option[Boolean],
                          configAddIdMapWrg: Option[Boolean],
                          configAddIdAfterReduce: Option[Boolean]
                        ) = LocalMemoryRulesSettings(
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.addIdForCurrentValueInReduce) else None, configAddIdAfterReduce, defaultAddIdForCurrentValueInReduce),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.addIdMapLcl) else None, configAddIdMapLcl, defaultAddIdMapLcl),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.addIdMapWrg) else None, configAddIdMapWrg, defaultAddIdMapWrg),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.addIdAfterReduce) else None, configAddIdAfterReduce, defaultAddIdAfterReduce))
}

case class ParameterRewriteSettings(
                                     exploreNDRange: Boolean,
                                     sampleNDRange: Int,
                                     disableNDRangeInjection: Boolean,
                                     sequential: Boolean,
                                     generateScala: Boolean
                                   ) extends ExplorationSettings {

  override val defaultParameters: ListMap[String, Any] = ParameterRewriteSettings.defaultParameters
  override val generateConfigString : String = ParameterRewriteSettings.generateConfigString
  override def toString: String =
    s"""${ParameterRewriteSettings.keyParameterRewriteSettings}:
       |    ${ParameterRewriteSettings.defaultParameters.mkString("\n")}
      """.stripMargin

}

object ParameterRewriteSettings {

  import SplitSlideRewrite._
  import exploration.utils.ExplorationParameter._
  private[exploration] val defaultExploreNDRange = false
  private[exploration] val defaultSampleNDRange = 0
  private[exploration] val defaultDisableNDRangeInjection = false
  private[exploration] val defaultSequential = false
  private[exploration] val defaultGenerateScala = false

  private[exploration] val keyParameterRewriteSettings = "parameter_rewrite_settings"
  private[exploration] val keyExploreNDRange = "explore_ndrange"
  private[exploration] val keySampleNDRange = "sample_ndrange"
  private[exploration] val keyDisableNDRangeInjection = "disable_ndrange_injection"
  private[exploration] val keySequential = "sequential"
  private[exploration] val keyGenerateScala = "generate_scala"

  private[exploration] val defaultParameters = ListMap(
    keyExploreNDRange -> defaultExploreNDRange,
    keySampleNDRange -> defaultSampleNDRange,
    keyDisableNDRangeInjection -> defaultDisableNDRangeInjection,
    keySequential -> defaultSequential,
    keyGenerateScala -> defaultGenerateScala
  )

  def generateConfigString = ExplorationParameter.generateConfigString(keyParameterRewriteSettings, defaultParameters)

  def createDefault = createWithDefaults(None, None, None, None, None)

  def createWithDefaults(
                          configExploreNDRange: Option[Boolean],
                          configSampleNDRange: Option[Int],
                          configDisableNDRangeInjection: Option[Boolean],
                          configSequential: Option[Boolean],
                          configGenerateScala: Option[Boolean]
                        ) = ParameterRewriteSettings(
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.exploreNDRange) else None, configExploreNDRange, defaultExploreNDRange),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.sampleNDRange) else None, configSampleNDRange, defaultSampleNDRange),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.disableNDRangeInjection) else None, configDisableNDRangeInjection, defaultDisableNDRangeInjection),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.sequential) else None, configSequential, defaultSequential),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.generateScala) else None, configGenerateScala, defaultGenerateScala))
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
                                       ) extends ExplorationSettings {
  override val defaultParameters: ListMap[String, Any] = MemoryMappingRewriteSettings.defaultParameters
  override val generateConfigString : String = MemoryMappingRewriteSettings.generateConfigString
  override def toString: String =
    s"""${MemoryMappingRewriteSettings.keyMemoryMappingRewrite}:
       |    ${MemoryMappingRewriteSettings.defaultParameters.mkString("\n")}
      """.stripMargin

}

object MemoryMappingRewriteSettings {

  import MemoryMappingRewrite._
  import exploration.utils.ExplorationParameter._

  protected[exploration] val defaultVectorize = true
  protected[exploration] val defaultVectorWidth = 4
  protected[exploration] val defaultSequential = false
  protected[exploration] val defaultLoadBalancing = false
  protected[exploration] val defaultUnrollReduce = false
  protected[exploration] val defaultGlobal0 = false
  protected[exploration] val defaultGlobal01 = false
  protected[exploration] val defaultGlobal10 = false
  protected[exploration] val defaultGlobal012 = false
  protected[exploration] val defaultGlobal210 = false
  protected[exploration] val defaultGroup0 = false
  protected[exploration] val defaultGroup01 = false
  protected[exploration] val defaultGroup10 = false

  protected[exploration] val keyMemoryMappingRewrite = "memory_mapping_rewrite"
  protected[exploration] val keyVectorize = "vectorize"
  protected[exploration] val keyVectorWidth = "vector_width"
  protected[exploration] val keySequential = "sequential"
  protected[exploration] val keyLoadBalancing = "load_balancing"
  protected[exploration] val keyUnrollReduce = "unroll_reduce"
  protected[exploration] val keyGlobal0 = "global0"
  protected[exploration] val keyGlobal01 = "global01"
  protected[exploration] val keyGlobal10 = "global10"
  protected[exploration] val keyGlobal012 = "global012"
  protected[exploration] val keyGlobal210 = "global210"
  protected[exploration] val keyGroup0 = "group0"
  protected[exploration] val keyGroup01 = "group01"
  protected[exploration] val keyGroup10 = "group10"

  protected[exploration] val defaultParameters = ListMap(
    keyVectorize -> defaultVectorize,
    keyVectorWidth -> defaultVectorWidth,
    keySequential -> defaultSequential,
    keyLoadBalancing -> defaultLoadBalancing,
    keyUnrollReduce -> defaultUnrollReduce,
    keyGlobal0 -> defaultGlobal0,
    keyGlobal01 -> defaultGlobal01,
    keyGlobal10 -> defaultGlobal10,
    keyGlobal012 -> defaultGlobal012,
    keyGlobal210 -> defaultGlobal210,
    keyGroup0 -> defaultGroup0,
    keyGroup01 -> defaultGroup01,
    keyGroup10 -> defaultGroup10
  )

  def generateConfigString = ExplorationParameter.generateConfigString(keyMemoryMappingRewrite, defaultParameters)

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
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.vectorize) else None, configVectorize, defaultVectorize),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.vectorWidth) else None, configVectorWidth, defaultVectorWidth),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.sequential) else None, configSequential, defaultSequential),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.loadBalancing) else None, configLoadBalancing, defaultLoadBalancing),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.unrollReduce) else None, configUnrollReduce, defaultUnrollReduce),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.global0) else None, configGlobal0, defaultGlobal0),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.global01) else None, configGlobal01, defaultGlobal01),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.global10) else None, configGlobal10, defaultGlobal10),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.global012) else None, configGlobal012, defaultGlobal012),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.global210) else None, configGlobal210, defaultGlobal210),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.group0) else None, configGroup0, defaultGroup0),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.group01) else None, configGroup01, defaultGroup01),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.group10) else None, configGroup10, defaultGroup10))
}


case class HighLevelRewriteSettings(
                                     explorationDepth: Int,
                                     depth: Int,
                                     distance: Int,
                                     ruleRepetition: Int,
                                     vectorWidth: Int,
                                     sequential: Boolean,
                                     onlyLower: Boolean,
                                     ruleCollection: String
                                   ) extends ExplorationSettings {
  override def toString: String =
    s"""${HighLevelRewriteSettings.keyHighLevelRewrite}:
       |    ${HighLevelRewriteSettings.defaultParameters.mkString("\n")}
      """.stripMargin

  override val defaultParameters: ListMap[String, Any] = HighLevelRewriteSettings.defaultParameters
  override def generateConfigString : String = HighLevelRewriteSettings.generateConfigString
}

object HighLevelRewriteSettings {

  import HighLevelRewrite._
  import exploration.utils.ExplorationParameter._
  protected[exploration] val keyHighLevelRewrite = "high_level_rewrite"

  protected[exploration] val keyExplorationDepth = "exploration_depth"
  protected[exploration] val keyDepthFilter = "depth_filter"
  protected[exploration] val keyDistanceFilter = "distance_filter"
  protected[exploration] val keyRuleRepetition = "rule_repetition"
  protected[exploration] val keyVectorWidth = "vector_width"
  protected[exploration] val keySequential = "sequential"
  protected[exploration] val keyOnlyLower = "only_lower"
  protected[exploration] val keyRuleCollection = "rule_collection"

  protected[exploration] val defaultExplorationDepth = 5
  protected[exploration] val defaultDepthFilter = 6
  protected[exploration] val defaultDistanceFilter = 1
  protected[exploration] val defaultRuleRepetition = 2
  protected[exploration] val defaultVectorWidth = 4
  protected[exploration] val defaultSequential = false
  protected[exploration] val defaultOnlyLower = false
  protected[exploration] val defaultRuleCollection = "default"

  protected[exploration] val defaultParameters = ListMap(
    keyExplorationDepth -> defaultExplorationDepth,
    keyDepthFilter -> defaultDepthFilter,
    keyDistanceFilter -> defaultDistanceFilter,
    keyRuleRepetition -> defaultRuleRepetition,
    keyVectorWidth -> defaultVectorWidth,
    keySequential -> defaultSequential,
    keyOnlyLower -> defaultOnlyLower,
    keyRuleCollection -> defaultRuleCollection
  )

  def generateConfigString: String = ExplorationParameter.generateConfigString(keyHighLevelRewrite, defaultParameters)

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None)

  def createWithDefaults(
                          configExplorationDepth: Option[Int],
                          configDepthFilter: Option[Int],
                          configDistanceFilter: Option[Int],
                          configRuleRepetition: Option[Int],
                          configVectorWidth: Option[Int],
                          configSequential: Option[Boolean],
                          configOnlyLower: Option[Boolean],
                          configRuleCollection: Option[String]
                        ) = HighLevelRewriteSettings(
    // priority: 1) command-line args; 2) config-file; 3) default values
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.explorationDepth) else None, configExplorationDepth, defaultExplorationDepth),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.depthFilter) else None, configDepthFilter, defaultDepthFilter),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.distanceFilter) else None, configDistanceFilter, defaultDistanceFilter),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.ruleRepetition) else None, configRuleRepetition, defaultRuleRepetition),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.vectorWidth) else None, configVectorWidth, defaultVectorWidth),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.sequential) else None, configSequential, defaultSequential),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.onlyLower) else None, configOnlyLower, defaultOnlyLower),
    getValue(if (cmdArgs.isDefined) Some(cmdArgs.get.ruleCollection) else None, configRuleCollection, defaultRuleCollection))
}

object SearchParameters {

  // Default input size for all dimensions to use for filtering, if no input combinations provided
  protected[exploration] val defaultInputSize = 1024
  // Minimum number of work item per workgroup
  protected[exploration] val defaultMinLocalSize = 128
  // Maximum number of work item per workgroup
  protected[exploration] val defaultMaxLocalSize = 1024
  // Minimal global grid size
  protected[exploration] val defaultMinGlobalSize = 8
  // Max amount of private memory allocated (this is not necessarily the number of registers)
  protected[exploration] val defaultMaxPrivateMemory = 1024
  // Max static amount of local memory
  protected[exploration] val defaultMaxLocalMemory = 50000
  // Minimum number of workgroups
  protected[exploration] val defaultMinWorkgroups = 8
  // Maximum number of workgroups
  protected[exploration] val defaultMaxWorkgroups = 10000

  protected[exploration] val keySearchParameters = "search_parameters"
  protected[exploration] val keyInputSize = "default_input_size"
  protected[exploration] val keyMinLocalSize = "min_local_size"
  protected[exploration] val keyMaxLocalSize = "max_local_size"
  protected[exploration] val keyMinGlobalSize = "min_global_size"
  protected[exploration] val keyMaxPrivateMemory = "max_private_memory"
  protected[exploration] val keyMaxLocalMemory = "max_local_memory"
  protected[exploration] val keyMinWorkgroups = "min_workgroups"
  protected[exploration] val keyMaxWorkgroups = "max_workgroups"

  protected[exploration] val defaultParameters = ListMap(
    keyInputSize -> defaultInputSize,
    keyMinLocalSize -> defaultMinLocalSize,
    keyMaxLocalSize -> defaultMaxLocalSize,
    keyMinGlobalSize -> defaultMinGlobalSize,
    keyMaxPrivateMemory -> defaultMaxPrivateMemory,
    keyMaxLocalMemory -> defaultMaxLocalMemory,
    keyMinWorkgroups -> defaultMinWorkgroups,
    keyMaxWorkgroups -> defaultMaxWorkgroups
  )

  def generateConfigString: String = ExplorationParameter.generateConfigString(keySearchParameters, defaultParameters)

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None)

  def createWithDefaults(
                          inputSize: Option[Int],
                          minLocalSize: Option[Int],
                          maxLocalSize: Option[Int],
                          minGlobalSize: Option[Int],
                          maxPrivateMemory: Option[Int],
                          maxLocalMemory: Option[Int],
                          minWorkgroups: Option[Int],
                          maxWorkgroups: Option[Int]
                        ) = SearchParameters(
    inputSize.getOrElse(defaultInputSize),
    minLocalSize.getOrElse(defaultMinLocalSize),
    maxLocalSize.getOrElse(defaultMaxLocalSize),
    minGlobalSize.getOrElse(defaultMinGlobalSize),
    maxPrivateMemory.getOrElse(defaultMaxPrivateMemory),
    maxLocalMemory.getOrElse(defaultMaxLocalMemory),
    minWorkgroups.getOrElse(defaultMinWorkgroups),
    maxWorkgroups.getOrElse(defaultMaxWorkgroups)
  )
}

case class SearchParameters(
                             inputSize: Int,
                             minLocalSize: Int,
                             maxLocalSize: Int,
                             minGlobalSize: Int,
                             maxPrivateMemory: Int,
                             maxLocalMemory: Int,
                             minWorkgroups: Int,
                             maxWorkgroups: Int
                           ) extends ExplorationSettings {
  override def toString: String =
    s"""${SearchParameters.keySearchParameters}:
       |    ${SearchParameters.defaultParameters.mkString("\n")}
      """.stripMargin

  override val defaultParameters: ListMap[String, Any] = SearchParameters.defaultParameters
  override def generateConfigString : String = SearchParameters.generateConfigString
}

case class Settings(
                     inputCombinations: Option[Seq[Seq[ArithExpr]]] = None,
                     searchParameters: SearchParameters = SearchParameters.createDefault,
                     highLevelRewriteSettings: HighLevelRewriteSettings = HighLevelRewriteSettings.createDefault,
                     memoryMappingRewriteSettings: MemoryMappingRewriteSettings = MemoryMappingRewriteSettings.createDefault,
                     parameterRewriteSettings: ParameterRewriteSettings = ParameterRewriteSettings.createDefault,
                     localMemoryRulesSettings: LocalMemoryRulesSettings = LocalMemoryRulesSettings.createDefault
                   ) extends ExplorationSettings {

  override def toString: String = {
    s"""Settings(
       |  $inputCombinations,
       |  $searchParameters
       |  $highLevelRewriteSettings
       |  $memoryMappingRewriteSettings
       |  $parameterRewriteSettings
       |  $localMemoryRulesSettings
       |)""".stripMargin
  }

  override val defaultParameters: ListMap[String, Any] = Settings.defaultParameters
  override def generateConfigString : String = Settings.generateConfigString
}

object Settings {
  private[exploration] val keyInputCombinations = "input_combinations"

  private[exploration] val defaultParameters = ListMap(
    SearchParameters.keySearchParameters -> SearchParameters.createDefault,
    HighLevelRewriteSettings.keyHighLevelRewrite -> HighLevelRewriteSettings.createDefault,
    MemoryMappingRewriteSettings.keyMemoryMappingRewrite -> MemoryMappingRewriteSettings.createDefault,
    ParameterRewriteSettings.keyParameterRewriteSettings -> ParameterRewriteSettings.createDefault,
    LocalMemoryRulesSettings.keyLocalMemoryRulesSettings -> LocalMemoryRulesSettings.createDefault
  )

  def generateConfigString : String = {
    "{\n" +
      ExplorationParameter.generateInnerJson(HighLevelRewriteSettings.keyHighLevelRewrite, HighLevelRewriteSettings.defaultParameters) + ",\n" +
      ExplorationParameter.generateInnerJson(MemoryMappingRewriteSettings.keyMemoryMappingRewrite, MemoryMappingRewriteSettings.defaultParameters) + ",\n" +
      ExplorationParameter.generateInnerJson(ParameterRewriteSettings.keyParameterRewriteSettings, ParameterRewriteSettings.defaultParameters) + ",\n" +
      ExplorationParameter.generateInnerJson(LocalMemoryRulesSettings.keyLocalMemoryRulesSettings, LocalMemoryRulesSettings.defaultParameters) + ",\n" +
      ExplorationParameter.generateInnerJson(SearchParameters.keySearchParameters, SearchParameters.defaultParameters) + "\n" +
    "}"

  }

}

object ParseSettings {

  private val logger = Logger(this.getClass)

  private[exploration] implicit val arithExprReads: Reads[ArithExpr] =
    JsPath.read[Long].map(Cst)

  val sp = SearchParameters
  private val parametersReads: Reads[SearchParameters] = (
    (JsPath \ sp.keyInputSize).readNullable[Int](min[Int](1)) and
      (JsPath \ sp.keyMinLocalSize).readNullable[Int](min[Int](1)) and
      (JsPath \ sp.keyMaxLocalSize).readNullable[Int](min[Int](1)) and
      (JsPath \ sp.keyMinGlobalSize).readNullable[Int](min[Int](1)) and
      (JsPath \ sp.keyMaxPrivateMemory).readNullable[Int](min[Int](1)) and
      (JsPath \ sp.keyMaxLocalMemory).readNullable[Int](min[Int](1)) and
      (JsPath \ sp.keyMinWorkgroups).readNullable[Int](min[Int](1)) and
      (JsPath \ sp.keyMaxWorkgroups).readNullable[Int](min[Int](1))
    ) (SearchParameters.createWithDefaults _)

  private[exploration] implicit val strictParameterReads = new Reads[SearchParameters] {
    def reads(jsv: JsValue) : JsResult[SearchParameters] =
      ExplorationParameter.reads[SearchParameters](
        jsv, parametersReads, sp.defaultParameters.keySet
      )
  }

  val hlr = HighLevelRewriteSettings
  private val highLevelReads: Reads[HighLevelRewriteSettings] = (
    (JsPath \ hlr.keyExplorationDepth).readNullable[Int](min[Int](1)) and
      (JsPath \ hlr.keyDepthFilter).readNullable[Int](min[Int](1)) and
      (JsPath \ hlr.keyDistanceFilter).readNullable[Int](min[Int](1)) and
      (JsPath \ hlr.keyRuleRepetition).readNullable[Int](min[Int](1)) and
      (JsPath \ hlr.keyVectorWidth).readNullable[Int](min[Int](1)) and
      (JsPath \ hlr.keySequential).readNullable[Boolean] and
      (JsPath \ hlr.keyOnlyLower).readNullable[Boolean] and
      (JsPath \ hlr.keyRuleCollection).readNullable[String]
    ) (HighLevelRewriteSettings.createWithDefaults _)

  private[exploration] implicit val strictHighLevelReads = new Reads[HighLevelRewriteSettings] {
    def reads(jsv: JsValue) : JsResult[HighLevelRewriteSettings] =
      ExplorationParameter.reads[HighLevelRewriteSettings](
        jsv, highLevelReads, hlr.defaultParameters.keySet
      )
  }

  val mmr = MemoryMappingRewriteSettings
  private val memoryMappingReads: Reads[MemoryMappingRewriteSettings] = (
    (JsPath \ mmr.keyVectorize).readNullable[Boolean] and
      (JsPath \ mmr.keyVectorWidth).readNullable[Int](min[Int](1)) and
      (JsPath \ mmr.keySequential).readNullable[Boolean] and
      (JsPath \ mmr.keyLoadBalancing).readNullable[Boolean] and
      (JsPath \ mmr.keyUnrollReduce).readNullable[Boolean] and
      (JsPath \ mmr.keyGlobal0).readNullable[Boolean] and
      (JsPath \ mmr.keyGlobal01).readNullable[Boolean] and
      (JsPath \ mmr.keyGlobal10).readNullable[Boolean] and
      (JsPath \ mmr.keyGlobal012).readNullable[Boolean] and
      (JsPath \ mmr.keyGlobal210).readNullable[Boolean] and
      (JsPath \ mmr.keyGroup0).readNullable[Boolean] and
      (JsPath \ mmr.keyGroup01).readNullable[Boolean] and
      (JsPath \ mmr.keyGroup10).readNullable[Boolean]
    ) (MemoryMappingRewriteSettings.createWithDefaults _)

  private[exploration] implicit val strictMemoryMappingReads = new Reads[MemoryMappingRewriteSettings] {
    def reads(jsv: JsValue) : JsResult[MemoryMappingRewriteSettings] =
      ExplorationParameter.reads[MemoryMappingRewriteSettings](
        jsv, memoryMappingReads, mmr.defaultParameters.keySet
      )
  }

  val pr = ParameterRewriteSettings
  private val parameterRewriteReads: Reads[ParameterRewriteSettings] = (
    (JsPath \ pr.keyExploreNDRange).readNullable[Boolean] and
      (JsPath \ pr.keySampleNDRange).readNullable[Int] and
      (JsPath \ pr.keyDisableNDRangeInjection).readNullable[Boolean] and
      (JsPath \ pr.keySequential).readNullable[Boolean] and
      (JsPath \ pr.keyGenerateScala).readNullable[Boolean]
    ) (ParameterRewriteSettings.createWithDefaults _)

  private[exploration] implicit val strictParameterRewriteReads = new Reads[ParameterRewriteSettings] {
    def reads(jsv: JsValue) : JsResult[ParameterRewriteSettings] =
      ExplorationParameter.reads[ParameterRewriteSettings](
        jsv, parameterRewriteReads, pr.defaultParameters.keySet
      )
  }

  val lmr = LocalMemoryRulesSettings
  private val localMemoryRulesReads: Reads[LocalMemoryRulesSettings] = (
    (JsPath \ lmr.keyAddIdForCurrentValueInReduce).readNullable[Boolean] and
      (JsPath \ lmr.keyAddIdMapLcl).readNullable[Boolean] and
      (JsPath \ lmr.keyAddIdMapWrg).readNullable[Boolean] and
      (JsPath \ lmr.keyAddIdAfterReduce).readNullable[Boolean]
    ) (LocalMemoryRulesSettings.createWithDefaults _)

  private[exploration] implicit val strictLocalMemoryReads = new Reads[LocalMemoryRulesSettings] {
    def reads(jsv: JsValue) : JsResult[LocalMemoryRulesSettings] =
      ExplorationParameter.reads[LocalMemoryRulesSettings](
        jsv, localMemoryRulesReads, lmr.defaultParameters.keySet
      )
  }

  private[exploration] implicit val settingsReads: Reads[Settings] = (
    (JsPath \ "input_combinations").readNullable[Seq[Seq[ArithExpr]]] and
      (JsPath \ SearchParameters.keySearchParameters).readNullable[SearchParameters] and
      (JsPath \ HighLevelRewriteSettings.keyHighLevelRewrite).readNullable[HighLevelRewriteSettings] and
      (JsPath \ MemoryMappingRewriteSettings.keyMemoryMappingRewrite).readNullable[MemoryMappingRewriteSettings] and
      (JsPath \ ParameterRewriteSettings.keyParameterRewriteSettings).readNullable[ParameterRewriteSettings] and
      (JsPath \ LocalMemoryRulesSettings.keyLocalMemoryRulesSettings).readNullable[LocalMemoryRulesSettings]
    ) ((maybeCombinations, maybeParameters, maybeHighLevel, maybeMemoryMapping, maybeParameterRewrite, maybeLocalMemoryRules) =>
    Settings(
      maybeCombinations,
      maybeParameters.getOrElse(SearchParameters.createDefault),
      maybeHighLevel.getOrElse(HighLevelRewriteSettings.createDefault),
      maybeMemoryMapping.getOrElse(MemoryMappingRewriteSettings.createDefault),
      maybeParameterRewrite.getOrElse(ParameterRewriteSettings.createDefault),
      maybeLocalMemoryRules.getOrElse(LocalMemoryRulesSettings.createDefault)
    ))

  def apply(optionFilename: Option[String]): Settings =
    optionFilename match {
      case Some(filename) =>

        val settingsString = SplitSlideRewrite.readFromFile(filename)
        val json = Json.parse(settingsString)
        val validated = json.validate[Settings]

        validated match {
          case JsSuccess(settings, _) =>
            checkSettings(settings)
            settings
          case e: JsError =>
            logger.error("Failed parsing settings " +
              e.recoverTotal(e => JsError.toJson(e)))
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
