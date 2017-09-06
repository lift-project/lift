package opencl.executor

import org.junit.{AfterClass, BeforeClass}

trait LoadExecutor {
  @BeforeClass
  def before(): Unit =
    Executor.loadAndInit()

  @AfterClass
  def after(): Unit =
    Executor.shutdown()
}
