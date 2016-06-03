package opencl.generator

object Debug {
  private var debug = System.getenv("APART_DEBUG") != null
  def apply() = debug
  def apply(debug: Boolean): Unit = { this.debug = debug }
}

object Verbose {
  private var verbose = System.getenv("APART_VERBOSE") != null
  def apply() = verbose
  def apply(verbose: Boolean): Unit = { this.verbose = verbose }
}

object DeadCodeElimination {
  private val dce = System.getenv("APART_DCE") != null
  def apply() = dce
}

object CSE {
  private val cse = System.getenv("APART_CSE") != null
  def apply() = cse
}

object PerformBarrierElimination {
  private val barrierElimination = System.getenv("APART_NO_BARRIER_ELIM") == null
  def apply() = barrierElimination
}

object PerformLoopOptimisation {
  private val loopOptimisation = System.getenv("APART_NO_LOOP_OPT") == null
  def apply() = loopOptimisation
}

object UseCastsForVectors {
  private val vectorCast = System.getenv("APART_VECTOR_CAST") == null
  def apply() = vectorCast
}

object AllocateLocalMemoryStatically {
  // FIXME(tlutz) This should be a val
  private var allocateLocalMemoryStatically = true
  def apply() = allocateLocalMemoryStatically
  def apply(allocateStatically: Boolean): Unit = {
    allocateLocalMemoryStatically = allocateStatically
  }
}

object OpenCL {
  val warpSize = 32
}
