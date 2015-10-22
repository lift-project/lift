package exploration.utils

import org.junit.Test
import org.junit.Assert._

class TestUtils {

  @Test
  def replaceVariableNames(): Unit = {
    val testString = "v__0, v__2135646"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(2, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring(): Unit = {
    val testString = "v__3, v__12, v__1)"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(3, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring2(): Unit = {
    val testString = "v__3, v__1, v__12"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(3, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring3(): Unit = {
    val testString = "Split((v__2*1/^(v__2739)))"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(2, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceVariableNamesUnrolled(): Unit = {
    val testString = "v__1, v__1_0, v__1_1"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(1, Utils.findVariables(replaced).length)
  }
}
