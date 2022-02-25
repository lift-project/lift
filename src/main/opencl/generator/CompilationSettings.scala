package opencl.generator


// TODO *********************
// TODO * Document all this *
// TODO *********************

object Debug {
  private var debug = System.getenv("LIFT_DEBUG") != null
  def apply(): Boolean = debug
  def apply(debug: Boolean): Unit = { this.debug = debug }
}

object TreatWarningsAsErrors {
  private var warningAsErrors = System.getenv("LIFT_TREAT_WARNINGS_AS_ERRORS") != null
  def apply(): Boolean = warningAsErrors
  def apply(warningAsErrors: Boolean): Unit = { this.warningAsErrors = warningAsErrors }
}

object InlineStructs {
  private var inline_structs = System.getenv("INLINE_STRUCTS") != null
  def apply(): Boolean = inline_structs
  def apply(inline_structs: Boolean): Unit = { this.inline_structs = inline_structs }
}

object Verbose {
  private var verbose = System.getenv("LIFT_VERBOSE") != null
  def apply(): Boolean = verbose
  def apply(verbose: Boolean): Unit = { this.verbose = verbose }
}

object StrictZip {
  // By default, zip is strict
  private val notStrict = System.getenv("LIFT_STRICT_ZIP") != null
  def apply(): Boolean = !notStrict
}

object DeadCodeElimination {
  private val dce = System.getenv("LIFT_DCE") != null
  def apply(): Boolean = dce
}

object CSE {
  private val cse = System.getenv("LIFT_CSE") != null
  def apply(): Boolean = cse
}

object EmptyCode {
  private val ec = System.getenv("LIFT_GENERATE_EMPTY_PATTERNS") != null
  def apply() : Boolean = ec
}

object PerformBarrierElimination {
  private var barrierElimination = System.getenv("LIFT_NO_BARRIER_ELIM") == null
  def apply(): Boolean = barrierElimination
  def set(b: Boolean): Unit = barrierElimination = b
}

object IgnoreBarrierFlags {
  private var ignoreBarrierFlags = System.getenv("LIFT_IGNORE_BARRIER_FLAGS") != null
  def apply(): Boolean = ignoreBarrierFlags
  def set(b: Boolean): Unit = ignoreBarrierFlags = b
}

object PerformBarrierInsertion {
  private var barrierInsertion = System.getenv("LIFT_BARRIER_INSERT") != null
  def apply(): Boolean = barrierInsertion
  def set(b: Boolean): Unit = barrierInsertion = b
}

object PerformLoopOptimisation {
  private val loopOptimisation = System.getenv("LIFT_NO_LOOP_OPT") == null
  def apply(): Boolean = loopOptimisation
}

object UseCastsForVectors {
  private val vectorCast = System.getenv("LIFT_VECTOR_CAST") != null
  def apply(): Boolean = vectorCast
}

object AllocateLocalMemoryStatically {
  private var allocateLocalMemoryStatically = true
  def apply(): Boolean = allocateLocalMemoryStatically
  def apply(allocateStatically: Boolean): Unit = {
    allocateLocalMemoryStatically = allocateStatically
  }
}

object OpenCL {
  val warpSize = 32
}

object MaliBifrost {
  val warpSize = 4
}
