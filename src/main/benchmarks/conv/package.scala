package benchmarks

import cbackends.common.WindowsOSInUse
import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import lift.arithmetic.{Cst, Var}
import opencl.generator.NDRange
import rewriting.utils.DumpToFile

import scala.concurrent.ExecutionContext
import scala.sys.process._
import scala.language.postfixOps

package object conv {
  private val logger = Logger(this.getClass)
  class NDRanges(private val _local: Option[NDRange],
                 val global: NDRange) {
    def local: NDRange = if (_local.isDefined) _local.get else global

    override def toString: String = s"Local: $local\nGlobal: $global"
  }

  object NDRanges {
    def apply(local: NDRange, global: NDRange): NDRanges = new NDRanges(Some(local), global)
    def apply(global: NDRange): NDRanges = new NDRanges(_local = None, global)
    def apply(): NDRanges = new NDRanges(_local = None, global = NDRange(1,1,1))
  }


  def getGitInfo: String = {
    val gitInfoCmd = "git log -n 1 && git diff"
    if (!WindowsOSInUse()) gitInfoCmd !!
    else ("cmd.exe /S /C " + gitInfoCmd) !!
  }

  def paramValuesAsCppMacroString(paramMacroNames: Predef.Map[Var, String],
                                  paramValues: Predef.Map[Var, Cst]): String =
    paramMacroNames.map {
      case (v: Var, macroName: String) => "-D " + macroName + "=" + paramValues(v).evalInt.toString
    }.mkString(" ")


  class MyCustomExecutionContext extends AnyRef with ExecutionContext {
    import ExecutionContext.Implicits.global
    @volatile var lastThread: Option[Thread] = None
    override def execute(runnable: Runnable): Unit = {
      ExecutionContext.Implicits.global.execute(new Runnable() {
        override def run() {
          lastThread = Some(Thread.currentThread)
          runnable.run()
        }
      })
    }
    override def reportFailure(t: Throwable): Unit = ???
  }

  implicit val exec: MyCustomExecutionContext = new MyCustomExecutionContext()

  def saveLambda(lambda: Either[Lambda, String], lambdaFileName: String): Unit = {
    val file = scala.tools.nsc.io.File(lambdaFileName)

    lambda match {
      case Left(l: Lambda) =>
        file.writeAll(DumpToFile.dumpLambdaToString(l))
      case Right(str: String) =>
        file.writeAll(str)
    }

    // Make sure it can still be evaluated
    //    Eval(readFromFile(lambdaFileName))
    logger.info(s"Saved a lambda in $lambdaFileName")
  }
}
