package opencl.executor

object Build {
  def apply(code: String): Kernel = {
    Kernel.create(code, "KERNEL", "").andPerform(_.build)
  }

  def apply(code: String, buildOptions: String): Kernel = {
    Kernel.create(code, "KERNEL", "buildOptions").andPerform(_.build)
  }

  // small trick to return the first argument
  // while performing a side effect afterwards
  implicit class AndThen(k: Kernel) {
    def andPerform[A](f: Kernel => A): Kernel = {
      f(k)
      k
    }
  }


}
