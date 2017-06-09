package exploration

import com.typesafe.scalalogging.Logger
import lift.arithmetic.{ArithExpr, Cst}
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class MemoryMappingRewriteSettings(
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
)

object MemoryMappingRewriteSettings {

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None, None, None, None, None)
  def createWithDefaults(
                        vectorWidth: Option[Int],
                        sequential: Option[Boolean],
                        loadBalancing: Option[Boolean],
                        unrollReduce: Option[Boolean],
                        global0: Option[Boolean],
                        global01: Option[Boolean],
                        global10: Option[Boolean],
                        global012: Option[Boolean],
                        global210: Option[Boolean],
                        group0: Option[Boolean],
                        group01: Option[Boolean],
                        group10: Option[Boolean]
                        ) = MemoryMappingRewriteSettings(
  vectorWidth.getOrElse(MemoryMappingRewrite.defaultVectorWidth),
  sequential.getOrElse(MemoryMappingRewrite.defaultSequential),
  loadBalancing.getOrElse(MemoryMappingRewrite.defaultLoadBalancing),
  unrollReduce.getOrElse(MemoryMappingRewrite.defaultUnrollReduce),
  global0.getOrElse(MemoryMappingRewrite.defaultGlobal0),
  global01.getOrElse(MemoryMappingRewrite.defaultGlobal01),
  global10.getOrElse(MemoryMappingRewrite.defaultGlobal10),
  global012.getOrElse(MemoryMappingRewrite.defaultGlobal012),
  global210.getOrElse(MemoryMappingRewrite.defaultGlobal210),
  group0.getOrElse(MemoryMappingRewrite.defaultGroup0),
  group01.getOrElse(MemoryMappingRewrite.defaultGroup01),
  group10.getOrElse(MemoryMappingRewrite.defaultGroup10))
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
)

object HighLevelRewriteSettings {

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None, None)
  def createWithDefaults(
                        explorationDepth: Option[Int],
                        depth: Option[Int],
                        distance: Option[Int],
                        ruleRepetition: Option[Int],
                        vectorWidth: Option[Int],
                        sequential: Option[Boolean],
                        onlyLower: Option[Boolean],
                        oldStringRepresentation: Option[Boolean],
                        ruleCollection: Option[String]
                        ) = HighLevelRewriteSettings(
  explorationDepth.getOrElse(HighLevelRewrite.defaultExplorationDepth),
  depth.getOrElse(HighLevelRewrite.defaultDepthFilter),
  distance.getOrElse(HighLevelRewrite.defaultDistanceFilter),
  ruleRepetition.getOrElse(HighLevelRewrite.defaultRuleRepetition),
  vectorWidth.getOrElse(HighLevelRewrite.defaultVectorWidth),
  sequential.getOrElse(HighLevelRewrite.defaultSequential),
  onlyLower.getOrElse(HighLevelRewrite.defaultOnlyLower),
  oldStringRepresentation.getOrElse(HighLevelRewrite.defaultOldStringRepresentation),
  ruleCollection.getOrElse(HighLevelRewrite.defaultRuleCollection))
}

object SearchParameters {
  // Default input size for all dimensions to use for filtering, if no input combinations provided
  private val default_size = 1024

  // Minimum number of work item per workgroup
  private val min_work_items = 128

  // Maximum number of work item per workgroup
  private val max_work_items = 1024

  // Minimal global grid size
  private val min_grid_size = 4

  // Max amount of private memory allocated (this is not necessarily the number of registers)
  private val max_amount_private_memory = 1024

  // Max static amount of local memory
  private val max_amount_local_memory = 50000

  // Minimum number of workgroups
  private val min_num_workgroups = 8

  // Maximum number of workgroups
  private val max_num_workgroups = 10000

  def createDefault = createWithDefaults(None, None, None, None, None, None, None, None)

  def createWithDefaults(
    defaultSize: Option[Int],
    minWorkItems: Option[Int],
    maxWorkItems: Option[Int],
    minGridSize: Option[Int],
    maxPrivateMemory: Option[Int],
    maxLocalMemory: Option[Int],
    minWorkgroups: Option[Int],
    maxWorkgroups: Option[Int]
  ) = SearchParameters(
    defaultSize.getOrElse(default_size),
    minWorkItems.getOrElse(min_work_items),
    maxWorkItems.getOrElse(max_work_items),
    minGridSize.getOrElse(min_grid_size),
    maxPrivateMemory.getOrElse(max_amount_private_memory),
    maxLocalMemory.getOrElse(max_amount_local_memory),
    minWorkgroups.getOrElse(min_num_workgroups),
    maxWorkgroups.getOrElse(max_num_workgroups)
  )

}

case class SearchParameters(
  defaultSize: Int,
  minWorkItems: Int,
  maxWorkItems: Int,
  minGridSize: Int,
  maxPrivateMemory: Int,
  maxLocalMemory: Int,
  minWorkgroups: Int,
  maxWorkgroups: Int
)

case class Settings(
  inputCombinations: Option[Seq[Seq[ArithExpr]]] = None,
  searchParameters: SearchParameters = SearchParameters.createDefault,
  highLevelRewriteSettings: HighLevelRewriteSettings = HighLevelRewriteSettings.createDefault,
  memoryMappingRewriteSettings: MemoryMappingRewriteSettings = MemoryMappingRewriteSettings.createDefault
) {

  override def toString: String = {
    s"""Settings(
       |  $inputCombinations,
       |  $searchParameters
       |  $highLevelRewriteSettings
       |  $memoryMappingRewriteSettings
       |)""".stripMargin
  }

}

object ParseSettings {

  private val logger = Logger(this.getClass)

  private[exploration] implicit val arithExprReads: Reads[ArithExpr] =
    JsPath.read[Long].map(Cst)

  private[exploration] implicit val parametersReads: Reads[SearchParameters] = (
    (JsPath \ "default_size").readNullable[Int] and
    (JsPath \ "min_work_items").readNullable[Int] and
    (JsPath \ "max_work_items").readNullable[Int] and
    (JsPath \ "min_grid_size").readNullable[Int] and
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

  private[exploration] implicit val settingsReads: Reads[Settings] = (
    (JsPath \ "input_combinations").readNullable[Seq[Seq[ArithExpr]]] and
    (JsPath \ "search_parameters").readNullable[SearchParameters] and
    (JsPath \ "high_level_rewrite").readNullable[HighLevelRewriteSettings] and
    (JsPath \ "memory_mapping_rewrite").readNullable[MemoryMappingRewriteSettings]
  )((maybeCombinations, maybeParameters, maybeHighLevel, maybeMemoryMapping) =>
    Settings(
      maybeCombinations,
      maybeParameters.getOrElse(SearchParameters.createDefault),
      maybeHighLevel.getOrElse(HighLevelRewriteSettings.createDefault),
      maybeMemoryMapping.getOrElse(MemoryMappingRewriteSettings.createDefault)
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
