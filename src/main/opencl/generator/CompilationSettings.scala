package opencl.generator

object Debug {
  var debug = System.getenv("APART_DEBUG") != null
  def apply() = debug
  def apply(debug: Boolean): Unit = { this.debug = debug }
}

object Verbose {
  var verbose = System.getenv("APART_VERBOSE") != null
  def apply() = verbose
  def apply(verbose: Boolean): Unit = { this.verbose = verbose }
}

object DeadCodeElimination {
  val dce = System.getenv("APART_DCE") != null
  def apply() = dce
}

object CSE {
  val cse = System.getenv("APART_CSE") != null
  def apply() = cse
}

object PerformBarrierElimination {
  val barrierElimination = System.getenv("APART_NO_BARRIER_ELIM") == null
  def apply() = barrierElimination
}

object PerformLoopOptimisation {
  val loopOptimisation = System.getenv("APART_NO_LOOP_OPT") == null
  def apply() = loopOptimisation
}

object AllocateLocalMemoryStatically {
  // FIXME(tlutz) This should be a val
  var allocateLocalMemoryStatically = true
  def apply() = allocateLocalMemoryStatically
  def apply(allocateStatically: Boolean): Unit = {
    allocateLocalMemoryStatically = allocateStatically
  }
}

object OpenCL {
  val warpSize = 32
}
