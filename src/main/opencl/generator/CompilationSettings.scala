package opencl.generator

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

object Verbose {
  private var verbose = System.getenv("LIFT_VERBOSE") != null
  def apply(): Boolean = verbose
  def apply(verbose: Boolean): Unit = { this.verbose = verbose }
}

object DeadCodeElimination {
  private val dce = System.getenv("LIFT_DCE") != null
  def apply(): Boolean = dce
}

object CSE {
  private val cse = System.getenv("LIFT_CSE") != null
  def apply(): Boolean = cse
}

object PerformBarrierElimination {
  private val barrierElimination = System.getenv("LIFT_NO_BARRIER_ELIM") == null
  def apply(): Boolean = barrierElimination
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
