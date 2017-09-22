package exploration

import ir.ast.Lambda
import lift.arithmetic.SizeVar
import org.junit.Assert._
import rewriting.rules.Rule
import rewriting.utils.Utils.getHash

package object algorithmic {

  val X = SizeVar("X")
  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")
  val v__1 = SizeVar("")
  val v__2 = SizeVar("")
  val v__3 = SizeVar("")
  val v__4 = SizeVar("")
  val v__5 = SizeVar("")
  val v__6 = SizeVar("")
  val v__7 = SizeVar("")
  val v__8 = SizeVar("")
  val v__9 = SizeVar("")
  val v__10 = SizeVar("")

  def checkDistance(lambda: Lambda): Unit =
    assertTrue(HighLevelRewrite.filterByDistance(HighLevelRewrite.finishRewriting(lambda)))

  def checkExists(gold: Lambda, lambdas: Seq[(Lambda, Seq[Rule])]): Unit = {
    val goldHash = getHash(gold)
    assertTrue(lambdas.exists(pair => getHash(pair._1) == goldHash))
  }
}
